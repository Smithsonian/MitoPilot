// Extract a reference database tarball ONCE per run, so every annotate / curate /
// orf task can symlink the shared read-only copy instead of copying and
// re-extracting ~200 MB apiece. Safe because the extracted DB is never written:
//   - MITOS (annotate) writes only to its MITOS2_temp* dirs, not the ref DB.
//   - Curation's remote BLAST hits go to a task-private DB (the split-DB change),
//     so the base featureProt DB is no longer mutated in place.
//
// The extracted DB lives in this process's work dir, which is inside the mounted
// work tree, so downstream symlinks resolve inside the container. (storeDir would
// persist it across runs too, but under Docker its cache dir is not mounted for
// the output-unstage move, so the task fails; a mounted cross-run cache is left as
// a future enhancement.) Deduplicating the input channel makes this run once per
// clade regardless.
process prepare_ref_db {

    // Copy the tarball in so it is a real, readable file (a symlinked input's
    // target is not mounted into the container). Runs once per clade, so the one
    // 45 MB copy is negligible.
    stageInMode 'copy'
    executor 'local'

    input:
    tuple val(clade), path(tarball)

    output:
    tuple val(clade), path("${clade}")

    shell:
    '''
    MIME_TYPE=$(file --mime-type -b "!{tarball}")
    if [[ "$MIME_TYPE" == "application/gzip" || "$MIME_TYPE" == "application/x-gzip" ]]; then
        # The ref DB tarballs ship a top-level "<clade>/" directory, so extracting
        # in place yields exactly the declared output. If a tarball ever lacks it,
        # gather everything under <clade> so the output is never empty.
        tar -xzf "!{tarball}"
        if [ ! -d "!{clade}" ]; then
            mkdir -p "!{clade}"
            for f in *; do
                case "$f" in "!{clade}"|"!{tarball}") ;; *) mv "$f" "!{clade}"/ ;; esac
            done
        fi
    else
        # Already a directory of ref files (no tarball): copy it under <clade>.
        mkdir -p "!{clade}"
        cp -rL "!{tarball}"/. "!{clade}"/
    fi
    '''
}
