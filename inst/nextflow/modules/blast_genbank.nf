process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

    // The BLAST target FASTA is a staged path input but is regenerated on every
    // run (deterministic content), so its last-modified time changes each time.
    // Default caching keys staged files on mtime, so -resume always re-ran this
    // slow remote BLAST. Lenient caching hashes staged inputs by name + size only
    // (ignoring mtime), so an unchanged target caches across resumes.
    cache 'lenient'

    maxForks params.blast_gb.maxForks

    // The local search has no network and no rate limit, and its failure modes
    // (unreadable database, unknown taxid, malformed extra_opts, a taxon
    // restriction the local search cannot honour) are deterministic: retrying
    // just repeats the same error. The shell exits 75 (EX_TEMPFAIL) for a REMOTE
    // search failure only, whether that came from the Remote BLAST toggle or the
    // no-hit fallback, and that is the status retried here with backoff.
    // Node failure / OOM / preemption signals get two attempts either way.
    // 'ignore' after retries keeps other samples running; failed tasks are NOT
    // cached as successful, so -resume will re-execute this step for the
    // affected sample.
    errorStrategy {
        def maxTries = (params.blast_gb.maxRetries instanceof Number) ? params.blast_gb.maxRetries : 3
        if (task.exitStatus == 75) {
            return task.attempt <= maxTries ? 'retry' : 'ignore'
        }
        // Nextflow reports Integer.MAX_VALUE when it cannot read .exitcode at all
        // (node crash, shared-filesystem lag, preemption). That is transient and
        // must stay retryable: without this it falls through to 'ignore' on the
        // first glitch, and every config sets failOnIgnore, so one hiccup aborts
        // the whole run.
        if (task.exitStatus == null || task.exitStatus == Integer.MAX_VALUE) {
            return task.attempt <= maxTries ? 'retry' : 'ignore'
        }
        return (task.exitStatus in [104, 134, 137, 139, 140, 143, 247] && task.attempt <= 2) ? 'retry' : 'ignore'
    }
    maxRetries { (params.blast_gb.maxRetries instanceof Number) ? params.blast_gb.maxRetries : 3 }

    // The local executor refuses to submit a task asking for more CPUs than the
    // machine has, and that refusal aborts the whole run rather than the task, so
    // clamp there. Schedulers allocate what they are asked for; leave those alone.
    cpus {
        def n = (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1
        (params.blast_gb.executor == 'local') ? Math.min(n, Runtime.runtime.availableProcessors()) : n
    }
    // Method-call form, NOT `memory = ...`. The assignment form is not a process
    // directive at all: the value is silently dropped and the generic process block
    // in .config wins instead (1 byte on config.local, and a hard task failure on
    // the scheduler templates, whose closure dereferences an `opts` process input
    // that this process does not have). Verified on Nextflow 26.04.
    memory { (params.blast_gb.memory instanceof Number) ? params.blast_gb.memory.GB : null }
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'



    tag "${id}.${path_idx}"

    input:
        tuple val(id), val(path_idx), path(assembly), val(opts_id), val(entrez_query), val(taxids), val(remote_blast), val(remote_fallback), val(extra_opts), val(max_target_seqs)

    output:
        tuple val(id), val(path_idx), path("${outDir}/${outFile}"), emit: hits
        path "${outDir}/blast_db_VERSION.txt", emit: db_version, optional: true

    shell:
    outDir = "${id}/assemble/${opts_id}"
    outFile = "blast_genbank_${path_idx}.txt"
    // Local database location. Defaulted here rather than required in .config:
    // backwards_compatibility(update_config = FALSE) and saved cluster profiles
    // both leave an existing .config without these keys.
    db_dir = params.blast_gb?.db_dir ?: '/ref_dbs/mito_metazoa'
    db_name = params.blast_gb?.db_name ?: 'mito_metazoa'
    // Per-parameter-set toggles, defaulted here so an old project database or an
    // old .config cannot crash the run.
    remote_on = ((remote_blast ?: 0).toString().trim().toLowerCase() in ['1', 'true', 'yes']) ? '1' : '0'
    fallback_on = ((remote_fallback == null ? 1 : remote_fallback).toString().trim().toLowerCase() in ['1', 'true', 'yes']) ? '1' : '0'
    // Omit -taxids entirely when unset: a blank value becomes a literal empty
    // flag, which blastn rejects as an invalid taxid list.
    tax_clean = (taxids ?: '').toString().replaceAll(/\s+/, '')
    tax_flag = tax_clean ? "-taxids ${tax_clean}" : ""
    // An Entrez query has no local equivalent. Treat the values that are no-ops
    // against a metazoan-mitogenome-only database as no-ops; anything else is a
    // real restriction, so refuse to run locally rather than silently drop it.
    eq_raw = (entrez_query ?: '').toString().trim()
    eq_norm = eq_raw.toLowerCase().replaceAll(/\s+/, ' ')
    eq_noop = eq_norm in ['', 'mitochondrion[location]', 'mitochondrion[filter]', 'biomol_genomic[prop]']
    blocked = (remote_on == '0' && !eq_noop) ? '1' : '0'
    // Remote Entrez query. -taxids is not accepted alongside -remote, so the
    // numeric IDs are translated to an exact Organism clause and AND-ed onto the
    // query: "7711" -> txid7711[Organism:exp], "7711,6656" ->
    // (txid7711[Organism:exp] OR txid6656[Organism:exp]). Numeric IDs only, so
    // this is an exact translation and the remote search honours the same taxon
    // restriction the local search would have applied.
    tax_list = tax_clean ? tax_clean.split(',').findAll { it } : []
    tax_entrez = tax_list ? (tax_list.size() == 1
        ? "txid${tax_list[0]}[Organism:exp]"
        : '(' + tax_list.collect { "txid${it}[Organism:exp]" }.join(' OR ') + ')') : ''
    // A blank Entrez query must NEVER reach the remote search: -remote with no
    // -entrez_query searches all of core_nt, so a nuclear or NUMT record can win
    // rank 1 and become the reference. Fall back to the historical default.
    eq_eff = eq_raw ?: 'mitochondrion[Location]'
    eq_remote = tax_entrez ? "${tax_entrez} AND ${eq_eff}" : eq_eff
    // Single-quote for the shell, escaping any embedded single quote. Entrez
    // queries routinely contain double quotes for multi-word organisms
    // ("Danio rerio"[Organism]), which would terminate a double-quoted argument
    // early and hand blastn split words; $ and backticks would also expand.
    entrez = "-entrez_query '" + eq_remote.replace("'", "'\\''") + "'"
    '''
    mkdir -p !{outDir}

    # The remote search is defined once and called from two places: the Remote
    # BLAST toggle below, and the no-hit fallback at the end of the local search.
    # It exits 75 (EX_TEMPFAIL) on failure, the one status the errorStrategy
    # retries, so a network hiccup still gets its backoff while a local failure
    # stays deterministic.
    run_remote() {
        # Back off on retries to give NCBI BLAST time to recover from rate limits
        if [ "!{task.attempt}" -gt 1 ]; then
            sleep $(( (!{task.attempt} - 1) * 60 ))
        fi
        # Optional NCBI API key raises remote BLAST rate limit. BLAST+ honors NCBI_API_KEY env var.
        export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
        # Distinguish a genuine zero-hit result from a connection/tool failure:
        #   - blastn non-zero exit          -> connection/tool error; exit 75 so Nextflow retries.
        #   - blastn exit 0, empty + stderr -> remote server error (e.g. NCBI queue DB
        #                                      failure) that does NOT set a non-zero exit;
        #                                      exit 75 so Nextflow retries.
        #   - blastn exit 0, empty, clean   -> genuine "no significant hits"; retrying is
        #                                      pointless, so write a sentinel and succeed.
        #                                      The workflow flags it with a distinct note.
        if blastn \
            -remote \
            -db core_nt \
            -query !{assembly} \
            -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
            -max_target_seqs !{max_target_seqs ?: 5} \
            -max_hsps 1 \
            -task megablast \
            !{entrez} \
            !{extra_opts} \
            > !{outDir}/!{outFile} 2> blast_remote.err; then
            # Same silent-discard guard as the local branch, for a -negative_taxids
            # (or similar) reaching blastn through extra_opts.
            if grep -qi 'requires additional data files' blast_remote.err; then
                cat blast_remote.err >&2
                echo "taxon restriction was silently discarded by blastn; refusing the result" >&2
                exit 1
            fi
            if [ ! -s !{outDir}/!{outFile} ]; then
                if grep -qiE 'error|bad_request|could not queue|failed|exception' blast_remote.err; then
                    cat blast_remote.err >&2
                    exit 75
                fi
                echo "NO_SIGNIFICANT_HITS" > !{outDir}/!{outFile}
            fi
        else
            cat blast_remote.err >&2
            exit 75
        fi
    }

    # Remote BLAST toggle: skip the local database entirely and search NCBI.
    if [ "!{remote_on}" = "1" ]; then
        run_remote
        exit 0
    fi

    if [ "!{blocked}" = "1" ]; then
        echo "This BLAST parameter set carries an Entrez query that the local BLAST" >&2
        echo "database cannot apply." >&2
        echo "The query is still saved even though the Entrez query field is hidden:" >&2
        echo "that field is only shown while Remote BLAST is ticked." >&2
        echo "Open BLAST Options, check Edit, and then either" >&2
        echo "  (a) tick Remote BLAST to search NCBI, where the query still applies, or" >&2
        echo "  (b) tick Remote BLAST to reveal the Entrez query field, clear it, then" >&2
        echo "      untick Remote BLAST, entering the restriction as numeric NCBI taxon" >&2
        echo "      IDs in the taxon ID field if you want to keep it." >&2
        echo "Click Update to save. Entering taxon IDs alone does NOT unblock this:" >&2
        echo "the Entrez query field itself has to be emptied." >&2
        exit 1
    fi

    # BLAST locates taxonomy4blast.sqlite3 (and taxdb.*) via BLASTDB. Without it,
    # -taxids / -negative_taxids do NOT error: blastn prints a notice to stderr,
    # DISCARDS the restriction, exits 0, and returns hits from every taxon.
    # Verified against this database, including with taxdb.btd/.bti present but
    # taxonomy4blast.sqlite3 absent, which is why the guard is on the sqlite3 file.
    export BLASTDB='!{db_dir}'
    if ! blastdbcmd -db "${BLASTDB}/!{db_name}" -info > /dev/null 2>&1; then
        echo "local BLAST database not readable: ${BLASTDB}/!{db_name}" >&2
        # Usually a container image that predates the bundled database: the R
        # package was upgraded but the project .config still names the old image
        # (backwards_compatibility(update_config = FALSE) leaves it alone, and
        # migrate_config deliberately preserves a custom container). Nothing is
        # silently dropped by going remote here, because the taxon restriction is
        # translated onto the Entrez query, so degrade rather than fail the run.
        if [ "!{fallback_on}" = "1" ]; then
            echo "searching NCBI remotely instead; update the container image in the" >&2
            echo "project .config to use the bundled local database" >&2
            run_remote
            exit 0
        fi
        exit 1
    fi
    # Only when a taxon restriction was actually requested. A site pointing
    # blast_gb.db_dir/db_name at its own database without the taxonomy files is
    # otherwise perfectly able to run unrestricted searches, and this guard would
    # hard-fail every sample. A -negative_taxids smuggled in through extra_opts
    # is still caught by the unconditional stderr check after blastn runs.
    if [ -n "!{tax_flag}" ] && [ ! -s "${BLASTDB}/taxonomy4blast.sqlite3" ]; then
        echo "${BLASTDB}/taxonomy4blast.sqlite3 is missing; the requested taxon" >&2
        echo "restriction would be silently discarded. Refusing to run." >&2
        exit 1
    fi

    rc=0
    blastn \
        -db "${BLASTDB}/!{db_name}" \
        -query !{assembly} \
        -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
        -max_target_seqs !{max_target_seqs ?: 5} \
        -max_hsps 1 \
        -task megablast \
        -num_threads !{task.cpus} \
        !{tax_flag} \
        !{extra_opts} \
        > !{outDir}/!{outFile} 2> blast.err || rc=$?

    # blastn exits 0 after silently discarding a taxon restriction it could not
    # apply. Checked unconditionally so it also covers a -negative_taxids passed
    # through extra_opts, which produces the identical notice.
    if grep -qi 'requires additional data files' blast.err; then
        cat blast.err >&2
        echo "taxon restriction was silently discarded by blastn; refusing the result" >&2
        exit 1
    fi

    if [ "$rc" -ne 0 ]; then
        cat blast.err >&2
        # A taxon restriction naming a clade with no sequence in this database is
        # a no-hit, not a tool failure, but blastn reports it as exit 2 rather
        # than returning zero rows. Treat it as an empty result so the remote
        # fallback below can honour the same restriction against NCBI, which is
        # exactly the case the fallback exists for.
        if grep -qi 'Taxonomy ID(s) not found' blast.err; then
            : > !{outDir}/!{outFile}
        else
            exit 1
        fi
    fi

    if [ -s !{outDir}/!{outFile} ]; then
        # Provenance, written only when the published hits really came from the
        # local database (not when the remote fallback below supplies them).
        cp "${BLASTDB}/VERSION" !{outDir}/blast_db_VERSION.txt 2>/dev/null || true
    else
        if [ "!{fallback_on}" = "1" ]; then
            # No significant hit locally: retry once against NCBI, which covers a
            # lineage with no mitogenome in the local database.
            echo "no significant hits in the local database; retrying the search remotely" >&2
            run_remote
        else
            echo "NO_SIGNIFICANT_HITS" > !{outDir}/!{outFile}
        fi
    fi
    '''
}
