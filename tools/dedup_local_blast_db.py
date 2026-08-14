#!/usr/bin/env python3
"""Collapse byte-identical sequences in the local mitogenome BLAST database.

GenBank holds both the RefSeq copy (NC_...) and the original submitter copy of
many mitogenomes. Remote core_nt only ever returned the RefSeq one, so a local
database containing both changes which accession wins rank 1 and wastes
candidate slots on the same genome under two names.

Everything here runs off the existing database via blastdbcmd. Nothing is
refetched from NCBI, so the deduplicated database cannot drift from the one that
was already verified.

Only byte-identical sequences collapse. A RefSeq copy that differs by one base,
or is rotated relative to its GenBank source, survives on purpose: merging those
would need alignment, and guessing wrong would silently delete a real reference.

Usage:
  python3 tools/dedup_local_blast_db.py [--workdir DIR] [--keep-source]
"""

import argparse
import hashlib
import os
import shutil
import subprocess
import sys
import tarfile
from datetime import datetime, timezone

DOCKER_IMAGE = os.environ.get("MITOPILOT_IMAGE", "mitopilot:1.5.1")
DB_NAME = "mito_metazoa"          # must not change: delivery depends on it
TAX_FILES = ["taxdb.btd", "taxdb.bti", "taxonomy4blast.sqlite3"]


def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)


def blast_cmd(workdir, args, stream=False):
    """Run a BLAST+ tool, locally if present, otherwise in the container."""
    if shutil.which(args[0]):
        cmd = args
    else:
        cmd = ["docker", "run", "--rm", "-v", f"{workdir}:/w", "-w", "/w",
               "-u", f"{os.getuid()}:{os.getgid()}",
               "-e", "BLASTDB=/w/db", DOCKER_IMAGE] + args
    if stream:
        return subprocess.Popen(cmd, cwd=workdir, stdout=subprocess.PIPE,
                                bufsize=1024 * 1024)
    return subprocess.run(cmd, cwd=workdir, check=True,
                          capture_output=True, text=True)


def pick_keeper(accs):
    """RefSeq wins; otherwise the lexicographically first accession.

    Preferring NC_ reproduces what the remote core_nt search returned, and it
    is the copy whose GFF3 carries curated annotation.
    """
    refseq = sorted(a for a in accs if a.startswith("NC_"))
    return refseq[0] if refseq else sorted(accs)[0]


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--workdir",
                   default="/media/dmacguig/Mockra/mitopilot_blastdb_build")
    p.add_argument("--keep-source", action="store_true",
                   help="keep the pre-dedup db/ directory instead of replacing it")
    opts = p.parse_args()

    wd = opts.workdir
    src_db = os.path.join(wd, "db")
    new_db = os.path.join(wd, "db_dedup")
    if not os.path.exists(os.path.join(src_db, f"{DB_NAME}.ndb")):
        sys.exit(f"no database at {src_db}/{DB_NAME}")
    os.makedirs(new_db, exist_ok=True)

    # 1. accession -> taxid, length
    log("dumping accession/taxid/length table")
    meta = {}
    res = blast_cmd(wd, ["blastdbcmd", "-db", f"db/{DB_NAME}", "-entry", "all",
                         "-outfmt", "%a\t%T\t%l"])
    for line in res.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) == 3:
            meta[parts[0]] = (parts[1], parts[2])
    log(f"  {len(meta)} records")

    # 2. hash every sequence, streamed so 2.8 Gbp never lands in memory
    log("hashing sequences")
    groups = {}
    proc = blast_cmd(wd, ["blastdbcmd", "-db", f"db/{DB_NAME}", "-entry", "all",
                          "-outfmt", "%a\t%s"], stream=True)
    seen = 0
    for raw in proc.stdout:
        line = raw.decode("utf-8", "replace").rstrip("\n")
        acc, _, seq = line.partition("\t")
        if not seq:
            continue
        h = hashlib.sha256(seq.upper().encode()).digest()
        groups.setdefault(h, []).append(acc)
        seen += 1
        if seen % 25000 == 0:
            log(f"  hashed {seen}/{len(meta)}")
    proc.stdout.close()
    if proc.wait() != 0:
        sys.exit("blastdbcmd sequence dump failed")
    log(f"  hashed {seen} sequences into {len(groups)} distinct sequences")

    # 3. choose keepers, record every drop
    keep, drops = [], []
    for accs in groups.values():
        if len(accs) == 1:
            keep.append(accs[0])
            continue
        keeper = pick_keeper(accs)
        keep.append(keeper)
        for a in accs:
            if a != keeper:
                taxid, slen = meta.get(a, ("", ""))
                drops.append((keeper, a, slen, taxid))

    keep.sort()
    log(f"keeping {len(keep)}, dropping {len(drops)} identical duplicates")
    nc_kept = sum(1 for a in keep if a.startswith("NC_"))
    nc_dropped = sum(1 for _, a, _, _ in drops if a.startswith("NC_"))
    log(f"  RefSeq kept {nc_kept}, RefSeq dropped {nc_dropped}")

    with open(os.path.join(wd, "dropped_duplicates.tsv"), "w") as fh:
        fh.write("kept\tdropped\tlength\ttaxid\n")
        for row in sorted(drops):
            fh.write("\t".join(row) + "\n")
    with open(os.path.join(wd, "keep_dedup.txt"), "w") as fh:
        fh.write("\n".join(keep) + "\n")
    with open(os.path.join(wd, "taxid_map_dedup.txt"), "w") as fh:
        for a in keep:
            fh.write(f"{a} {meta.get(a, ('0',))[0]}\n")

    # 4. extract the kept sequences and rebuild
    log("extracting kept sequences")
    fa = os.path.join(wd, f"{DB_NAME}_dedup.fa")
    with open(fa, "wb") as out:
        proc = blast_cmd(wd, ["blastdbcmd", "-db", f"db/{DB_NAME}",
                              "-entry_batch", "keep_dedup.txt"], stream=True)
        shutil.copyfileobj(proc.stdout, out)
        proc.stdout.close()
        if proc.wait() != 0:
            sys.exit("blastdbcmd extraction failed")

    got = bases = 0
    with open(fa) as fh:
        for line in fh:
            if line.startswith(">"):
                got += 1
            else:
                bases += len(line.strip())
    log(f"  extracted {got} sequences, {bases/1e9:.2f} Gbp")
    if got != len(keep):
        sys.exit(f"extraction mismatch: expected {len(keep)}, got {got}")

    log("running makeblastdb")
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    blast_cmd(wd, ["makeblastdb", "-in", f"{DB_NAME}_dedup.fa", "-dbtype", "nucl",
                   "-parse_seqids", "-blastdb_version", "5",
                   "-taxid_map", "taxid_map_dedup.txt",
                   "-out", f"db_dedup/{DB_NAME}",
                   "-title", f"MitoPilot metazoan mitogenomes {stamp} dedup"])

    # 5. taxonomy files. taxonomy4blast.sqlite3 is the load-bearing one: without
    # it a -taxids restriction is silently discarded with exit 0.
    for f in TAX_FILES:
        src = os.path.join(src_db, f)
        if not os.path.exists(src):
            sys.exit(f"missing {f} in {src_db}; taxon filtering would silently "
                     f"fail at run time")
        shutil.copy2(src, os.path.join(new_db, f))
    log("  copied taxonomy files")

    with open(os.path.join(new_db, "VERSION"), "w") as fh:
        old = {}
        old_path = os.path.join(src_db, "VERSION")
        if os.path.exists(old_path):
            for line in open(old_path):
                k, _, v = line.rstrip("\n").partition("\t")
                old[k] = v
        old.update({
            "built": datetime.now(timezone.utc).isoformat(),
            "sequences": str(got),
            "bases": str(bases),
            "deduplicated_identical": "True",
            "duplicates_dropped": str(len(drops)),
        })
        for k, v in old.items():
            fh.write(f"{k}\t{v}\n")

    tarball = os.path.join(wd, f"{DB_NAME}_blastdb.tar.gz")
    log("compressing")
    with tarfile.open(tarball + ".tmp", "w:gz") as tar:
        tar.add(new_db, arcname=DB_NAME)
    os.replace(tarball + ".tmp", tarball)

    db_bytes = sum(os.path.getsize(os.path.join(new_db, f))
                   for f in os.listdir(new_db))
    log(f"db_dedup {db_bytes/1e6:.0f} MB, tarball "
        f"{os.path.getsize(tarball)/1e6:.0f} MB")

    if not opts.keep_source:
        log("replacing db/ with db_dedup/ (previous kept as db_prededup/)")
        os.replace(src_db, os.path.join(wd, "db_prededup"))
        os.replace(new_db, src_db)
    log("done")


if __name__ == "__main__":
    main()
