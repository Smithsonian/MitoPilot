#!/usr/bin/env python3
"""Build a local BLAST database of metazoan mitochondrial genomes.

Maintainer-side batch job. Implements the inclusion rules in
tools/local_blast_db_design.md. Standard library only, no external deps.

Stages, each resumable (rerun the script and it skips finished work):
  1. uids     collect nuccore UIDs for the base query
  2. summary  page esummary, record accession/taxid/length/title/CDS count
  3. filter   apply the inclusion rules, write keep list + taxid map
  4. fasta    efetch sequence for kept accessions
  5. build    makeblastdb, VERSION file, tarball

Usage:
  export NCBI_API_KEY=...            # optional, raises rate limit 3/s -> 10/s
  python3 tools/build_local_blast_db.py [--workdir DIR] [--stage STAGE]

Status for external monitoring is written to WORKDIR/status.txt.
"""

import argparse
import gzip
import html
import os
import re
import shutil
import subprocess
import sys
import tarfile
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone

EUTILS = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
BASE_QUERY = "Metazoa[Organism] AND mitochondrion[filter]"


def search_term(opts):
    """Base query plus the length window.

    The length bound MUST be in the Entrez term. Without it the base query
    matches ~8M records (every COI barcode in GenBank) and the summary stage
    would page all of them only to discard 97%. Bounding the search leaves
    ~205k candidates. The lower bound is the more permissive of the two floors;
    the split floor and every other rule are applied locally in stage_filter.
    """
    lo = min(opts.min_len_any, opts.min_len_complete)
    return f"{BASE_QUERY} AND {lo}:{opts.max_len}[SLEN]"

# Defaults match tools/local_blast_db_design.md
DEFAULTS = dict(
    min_len_any=12000,
    min_len_complete=8000,
    max_len=100000,
    min_cds=1,
    drop_unverified=True,
)

UID_PAGE = 10000     # esearch retmax per request
SUM_CHUNK = 500      # esummary ids per request
FA_CHUNK = 200       # efetch ids per request
MAX_RETRIES = 5

STATUS = {"stage": "starting", "detail": ""}


# ---------------------------------------------------------------- utilities

def log(msg):
    stamp = datetime.now().strftime("%H:%M:%S")
    print(f"[{stamp}] {msg}", flush=True)


def set_status(workdir, stage, detail=""):
    STATUS["stage"], STATUS["detail"] = stage, detail
    tmp = os.path.join(workdir, "status.txt.tmp")
    with open(tmp, "w") as fh:
        fh.write(f"stage: {stage}\ndetail: {detail}\n"
                 f"updated: {datetime.now(timezone.utc).isoformat()}\n")
    os.replace(tmp, os.path.join(workdir, "status.txt"))


def api_key():
    return os.environ.get("NCBI_API_KEY", "").strip()


def throttle():
    # NCBI allows 3 req/s without a key, 10 req/s with one. Stay under.
    time.sleep(0.12 if api_key() else 0.40)


def eutils(endpoint, params, post=False):
    """One E-utilities call with retry and backoff. Returns text."""
    params = dict(params)
    if api_key():
        params["api_key"] = api_key()
    params.setdefault("tool", "MitoPilot_blastdb_build")
    params.setdefault("email", os.environ.get("NCBI_EMAIL", "mitopilot@example.org"))
    url = f"{EUTILS}/{endpoint}"
    data = urllib.parse.urlencode(params).encode()

    last = None
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            throttle()
            if post:
                req = urllib.request.Request(url, data=data)
            else:
                req = urllib.request.Request(f"{url}?{data.decode()}")
            with urllib.request.urlopen(req, timeout=180) as resp:
                body = resp.read()
            if body[:2] == b"\x1f\x8b":
                body = gzip.decompress(body)
            text = body.decode("utf-8", "replace")
            # NCBI returns HTTP 200 with an error body more often than it should
            if "<ERROR>" in text or "Error occurred" in text:
                raise RuntimeError(text[:300])
            return text
        except Exception as exc:      # noqa: BLE001 - retry anything transient
            last = exc
            wait = min(60, 5 * attempt * attempt)
            log(f"  retry {attempt}/{MAX_RETRIES} on {endpoint} in {wait}s: "
                f"{str(exc)[:120]}")
            time.sleep(wait)
    raise RuntimeError(f"{endpoint} failed after {MAX_RETRIES} attempts: {last}")


def chunked(seq, n):
    for i in range(0, len(seq), n):
        yield seq[i:i + n]


# ------------------------------------------------------------ stage 1: uids

def stage_uids(workdir, opts):
    out = os.path.join(workdir, "uids.txt")
    if os.path.exists(out) and os.path.getsize(out):
        uids = open(out).read().split()
        log(f"stage 1 uids: reusing {len(uids)} uids")
        return uids

    term = search_term(opts)
    set_status(workdir, "uids", "counting")
    log(f"stage 1 uids: query = {term}")
    text = eutils("esearch.fcgi", dict(db="nuccore", term=term, retmax=0))
    total = int(re.search(r"<Count>(\d+)</Count>", text).group(1))
    log(f"stage 1 uids: {total} candidate records")
    if total > 1_000_000:
        raise SystemExit(
            f"refusing to page {total} records: the length bound is missing or "
            f"wrong. Expected roughly 200k candidates.")

    uids = []
    for start in range(0, total, UID_PAGE):
        set_status(workdir, "uids", f"{len(uids)}/{total}")
        text = eutils("esearch.fcgi", dict(db="nuccore", term=term,
                                           retstart=start, retmax=UID_PAGE))
        page = re.findall(r"<Id>(\d+)</Id>", text)
        uids.extend(page)
        log(f"  uids {len(uids)}/{total}")
        if not page:
            break

    uids = list(dict.fromkeys(uids))
    with open(out + ".tmp", "w") as fh:
        fh.write("\n".join(uids) + "\n")
    os.replace(out + ".tmp", out)
    log(f"stage 1 uids: wrote {len(uids)} uids")
    return uids


# --------------------------------------------------------- stage 2: summary

REC_SPLIT = re.compile(r"<DocumentSummary ")
RE_ACC = re.compile(r"<AccessionVersion>([^<]+)</AccessionVersion>")
RE_CAP = re.compile(r"<Caption>([^<]+)</Caption>")
RE_TITLE = re.compile(r"<Title>(.*?)</Title>", re.S)
RE_SLEN = re.compile(r"<Slen>(\d+)</Slen>")
RE_TAXID = re.compile(r"<TaxId>(\d+)</TaxId>")
RE_CDS = re.compile(r'type="cdregion" count="(\d+)"')


def parse_summary(xml):
    rows = []
    for rec in REC_SPLIT.split(xml)[1:]:
        acc = RE_ACC.search(rec) or RE_CAP.search(rec)
        if not acc:
            continue
        title = RE_TITLE.search(rec)
        slen = RE_SLEN.search(rec)
        taxid = RE_TAXID.search(rec)
        cds = RE_CDS.search(rec)
        rows.append((
            acc.group(1),
            taxid.group(1) if taxid else "0",
            slen.group(1) if slen else "0",
            cds.group(1) if cds else "0",
            html.unescape(title.group(1)).replace("\t", " ").replace("\n", " ")
            if title else "",
        ))
    return rows


def stage_summary(workdir, uids):
    out = os.path.join(workdir, "summary.tsv")
    progress = os.path.join(workdir, "summary.progress")

    # Resume position is the count of COMPLETED CHUNKS, recorded explicitly.
    # It must not be inferred from the row count: esummary returns fewer rows
    # than requested for some chunks, so rows//SUM_CHUNK under-reports progress
    # and refetches a chunk already on disk, duplicating records. Duplicate
    # accessions make makeblastdb fail outright ("Duplicate seq_ids are found").
    start_chunk = 0
    if os.path.exists(progress) and os.path.exists(out):
        try:
            start_chunk = int(open(progress).read().strip())
        except ValueError:
            start_chunk = 0

    chunks = list(chunked(uids, SUM_CHUNK))
    if start_chunk >= len(chunks):
        log(f"stage 2 summary: reusing {len(chunks)} completed chunks")
        return out

    mode = "a" if start_chunk else "w"
    with open(out, mode) as fh:
        if not start_chunk:
            fh.write("accession\ttaxid\tslen\tcds\ttitle\n")
        for i in range(start_chunk, len(chunks)):
            xml = eutils("esummary.fcgi",
                         dict(db="nuccore", id=",".join(chunks[i]), version="2.0"),
                         post=True)
            rows = parse_summary(xml)
            for r in rows:
                fh.write("\t".join(r) + "\n")
            fh.flush()
            os.fsync(fh.fileno())
            with open(progress, "w") as pf:
                pf.write(str(i + 1))
            pct = 100 * (i + 1) / len(chunks)
            set_status(workdir, "summary", f"chunk {i+1}/{len(chunks)} ({pct:.0f}%)")
            if (i + 1) % 20 == 0 or i + 1 == len(chunks):
                log(f"  summary chunk {i+1}/{len(chunks)} ({pct:.0f}%)")
    log("stage 2 summary: done")
    return out


# ---------------------------------------------------------- stage 3: filter

RE_COMPLETE = re.compile(r"complete genome", re.I)
RE_NEARLY = re.compile(r"nearly\s+complete", re.I)
RE_UNVERIFIED = re.compile(r"^\s*UNVERIFIED", re.I)


def claims_complete(title):
    return bool(RE_COMPLETE.search(title)) and not RE_NEARLY.search(title)


def stage_filter(workdir, opts):
    summary = os.path.join(workdir, "summary.tsv")
    keep_path = os.path.join(workdir, "keep.txt")
    map_path = os.path.join(workdir, "taxid_map.txt")
    rej_path = os.path.join(workdir, "rejected.tsv")
    set_status(workdir, "filter", "")

    counts = dict(total=0, kept=0, unverified=0, too_short=0, too_long=0,
                  no_annotation=0, duplicate=0)
    seen = set()
    with open(summary) as fh, open(keep_path, "w") as keep, \
            open(map_path, "w") as tmap, open(rej_path, "w") as rej:
        next(fh)
        rej.write("accession\treason\tslen\tcds\ttitle\n")
        for line in fh:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 5:
                continue
            acc, taxid, slen, cds, title = parts[0], parts[1], parts[2], parts[3], parts[4]
            # Never emit an accession twice: makeblastdb rejects the whole build
            # on a duplicate seq_id.
            if acc in seen:
                counts["duplicate"] += 1
                continue
            seen.add(acc)
            slen, cds = int(slen or 0), int(cds or 0)
            counts["total"] += 1

            reason = None
            if opts.drop_unverified and RE_UNVERIFIED.search(title):
                reason = "unverified"
            elif slen > opts.max_len:
                reason = "too_long"
            else:
                floor = (opts.min_len_complete if claims_complete(title)
                         else opts.min_len_any)
                if slen < floor:
                    reason = "too_short"
                elif cds < opts.min_cds:
                    reason = "no_annotation"

            if reason:
                counts[reason] = counts.get(reason, 0) + 1
                rej.write(f"{acc}\t{reason}\t{slen}\t{cds}\t{title}\n")
            else:
                counts["kept"] += 1
                keep.write(acc + "\n")
                tmap.write(f"{acc} {taxid}\n")

    log("stage 3 filter: " + "  ".join(f"{k}={v}" for k, v in counts.items()))
    with open(os.path.join(workdir, "filter_counts.txt"), "w") as fh:
        for k, v in counts.items():
            fh.write(f"{k}\t{v}\n")
    return keep_path


# ----------------------------------------------------------- stage 4: fasta

def stage_fasta(workdir, keep_path):
    accs = [a.strip() for a in open(keep_path) if a.strip()]
    fa_dir = os.path.join(workdir, "fasta")
    os.makedirs(fa_dir, exist_ok=True)
    chunks = list(chunked(accs, FA_CHUNK))
    log(f"stage 4 fasta: {len(accs)} accessions in {len(chunks)} chunks")

    for i, chunk in enumerate(chunks):
        part = os.path.join(fa_dir, f"chunk_{i:05d}.fa")
        if os.path.exists(part):
            # Validate by ACCESSION SET, not record count. Chunks are keyed by
            # position in keep.txt, so if keep.txt ever changes the boundaries
            # shift and a cached file can hold the right number of the wrong
            # sequences. Counting alone would accept that silently.
            with open(part) as fh:
                cached = {line[1:].split()[0] for line in fh
                          if line.startswith(">") and len(line) > 1}
            if cached == set(chunk):
                continue                          # already complete and correct
            log(f"  chunk {i} stale, refetching")
        for attempt in range(1, MAX_RETRIES + 1):
            text = eutils("efetch.fcgi",
                          dict(db="nuccore", id=",".join(chunk),
                               rettype="fasta", retmode="text"),
                          post=True)
            got = text.count("\n>") + (1 if text.startswith(">") else 0)
            if got == len(chunk):
                break
            log(f"  chunk {i} short: {got}/{len(chunk)}, refetching "
                f"({attempt}/{MAX_RETRIES})")
            time.sleep(5 * attempt)
        else:
            raise RuntimeError(f"chunk {i} never returned {len(chunk)} records")
        with open(part + ".tmp", "w") as fh:
            fh.write(text if text.endswith("\n") else text + "\n")
        os.replace(part + ".tmp", part)

        pct = 100 * (i + 1) / len(chunks)
        set_status(workdir, "fasta", f"chunk {i+1}/{len(chunks)} ({pct:.0f}%)")
        if (i + 1) % 25 == 0 or i + 1 == len(chunks):
            log(f"  fasta chunk {i+1}/{len(chunks)} ({pct:.0f}%)")

    merged = os.path.join(workdir, "mito_metazoa.fa")
    log("stage 4 fasta: merging")
    seqs = bases = 0
    with open(merged + ".tmp", "w") as out:
        for i in range(len(chunks)):
            with open(os.path.join(fa_dir, f"chunk_{i:05d}.fa")) as fh:
                for line in fh:
                    if not line.strip():
                        continue
                    if line.startswith(">"):
                        seqs += 1
                    else:
                        bases += len(line.strip())
                    out.write(line)
    os.replace(merged + ".tmp", merged)
    log(f"stage 4 fasta: {seqs} sequences, {bases/1e9:.2f} Gbp")
    return merged, seqs, bases


# ----------------------------------------------------------- stage 5: build

DOCKER_IMAGE = os.environ.get("MITOPILOT_IMAGE", "mitopilot:1.5.1")


def run_makeblastdb(workdir, fasta, dbdir, title):
    os.makedirs(dbdir, exist_ok=True)
    args = ["-in", os.path.basename(fasta), "-dbtype", "nucl", "-parse_seqids",
            "-blastdb_version", "5", "-taxid_map", "taxid_map.txt",
            "-out", "db/mito_metazoa", "-title", title]
    if subprocess.run(["which", "makeblastdb"], capture_output=True).returncode == 0:
        cmd = ["makeblastdb"] + args
        log("stage 5 build: local makeblastdb")
        subprocess.run(cmd, cwd=workdir, check=True)
    else:
        log(f"stage 5 build: makeblastdb via docker ({DOCKER_IMAGE})")
        subprocess.run(
            ["docker", "run", "--rm", "-v", f"{workdir}:/w", "-w", "/w",
             "-u", f"{os.getuid()}:{os.getgid()}", DOCKER_IMAGE,
             "makeblastdb"] + args,
            check=True)


TAXDB_URL = "https://ftp.ncbi.nlm.nih.gov/blast/db/taxdb.tar.gz"


def fetch_taxdb(dbdir):
    """Install taxdb next to the database.

    Required for -taxids, which replaces the remote search's -entrez_query.
    Without these files blastn does NOT error out: it prints a notice, ignores
    the restriction, exits 0, and returns hits from every taxon. BLAST must
    also be able to find them, which means BLASTDB must point at this
    directory at run time.

    taxonomy4blast.sqlite3 is the load-bearing file, not taxdb.btd. Verified:
    with taxdb.btd and taxdb.bti present but the sqlite file absent, a query
    restricted to Arthropoda returned chordate hits and exit 0. Presence checks
    must therefore cover all three files.
    """
    needed = ["taxdb.btd", "taxdb.bti", "taxonomy4blast.sqlite3"]
    if all(os.path.exists(os.path.join(dbdir, f)) for f in needed):
        log("stage 5 build: taxdb already present")
        return
    log("stage 5 build: fetching taxdb")
    tgz = os.path.join(dbdir, "taxdb.tar.gz")
    urllib.request.urlretrieve(TAXDB_URL, tgz)
    with tarfile.open(tgz) as tar:
        tar.extractall(dbdir)
    os.remove(tgz)


def stage_build(workdir, fasta, seqs, bases, opts):
    set_status(workdir, "build", "makeblastdb")
    dbdir = os.path.join(workdir, "db")
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    run_makeblastdb(workdir, fasta, dbdir,
                    f"MitoPilot metazoan mitogenomes {stamp}")
    fetch_taxdb(dbdir)

    version = os.path.join(dbdir, "VERSION")
    with open(version, "w") as fh:
        fh.write(
            f"name\tmito_metazoa\n"
            f"built\t{datetime.now(timezone.utc).isoformat()}\n"
            f"source\tGenBank nuccore via E-utilities\n"
            f"query\t{search_term(opts)}\n"
            f"sequences\t{seqs}\n"
            f"bases\t{bases}\n"
            f"min_len_any\t{opts.min_len_any}\n"
            f"min_len_complete\t{opts.min_len_complete}\n"
            f"max_len\t{opts.max_len}\n"
            f"min_cds\t{opts.min_cds}\n"
            f"drop_unverified\t{opts.drop_unverified}\n")

    db_bytes = sum(os.path.getsize(os.path.join(dbdir, f))
                   for f in os.listdir(dbdir))
    tarball = os.path.join(workdir, "mito_metazoa_blastdb.tar.gz")
    set_status(workdir, "build", "compressing")
    with tarfile.open(tarball, "w:gz") as tar:
        tar.add(dbdir, arcname="mito_metazoa")

    log(f"stage 5 build: db {db_bytes/1e6:.0f} MB, "
        f"tarball {os.path.getsize(tarball)/1e6:.0f} MB")
    log("stage 5 build: set BLASTDB to the unpacked directory at run time, "
        "or -taxids will be silently ignored")
    set_status(workdir, "complete",
               f"{seqs} seqs, db {db_bytes/1e6:.0f} MB, "
               f"tarball {os.path.getsize(tarball)/1e6:.0f} MB")
    return tarball


# ------------------------------------------------------------------- driver

def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--workdir", default=os.path.expanduser("~/mitopilot_blastdb_build"))
    p.add_argument("--stage", choices=["uids", "summary", "filter", "fasta", "build", "all"],
                   default="all", help="run from this stage onward")
    p.add_argument("--resume", action="store_true",
                   help="continue an interrupted build in an existing workdir "
                        "instead of clearing it. Only safe when the query and "
                        "filter parameters are unchanged.")
    p.add_argument("--min-len-any", type=int, default=DEFAULTS["min_len_any"])
    p.add_argument("--min-len-complete", type=int, default=DEFAULTS["min_len_complete"])
    p.add_argument("--max-len", type=int, default=DEFAULTS["max_len"])
    p.add_argument("--min-cds", type=int, default=DEFAULTS["min_cds"])
    p.add_argument("--keep-unverified", dest="drop_unverified",
                   action="store_false", default=DEFAULTS["drop_unverified"])
    opts = p.parse_args()

    # A rerun starts from an empty workdir unless --resume is given. Every stage's
    # "already done" test is bare file existence, with no record of the query or
    # the filter parameters that produced it, so reusing a workdir silently
    # rebuilds the OLD data while stamping the NEW parameters into VERSION, which
    # is then copied into every user's project as provenance. Nothing of value is
    # lost by clearing: the shipped database lives in the container image, and the
    # tarball is staged separately for the image build.
    if os.path.isdir(opts.workdir) and os.listdir(opts.workdir) and not opts.resume:
        log(f"clearing {opts.workdir} (pass --resume to continue an interrupted build)")
        for name in os.listdir(opts.workdir):
            target = os.path.join(opts.workdir, name)
            if os.path.isdir(target):
                shutil.rmtree(target)
            else:
                os.remove(target)
    os.makedirs(opts.workdir, exist_ok=True)
    log(f"workdir {opts.workdir}")
    log(f"api key {'set' if api_key() else 'NOT set (3 req/s)'}")

    order = ["uids", "summary", "filter", "fasta", "build"]
    first = 0 if opts.stage == "all" else order.index(opts.stage)

    uids = stage_uids(opts.workdir, opts)
    if first <= 1:
        stage_summary(opts.workdir, uids)
    keep = stage_filter(opts.workdir, opts) if first <= 2 else \
        os.path.join(opts.workdir, "keep.txt")
    fasta, seqs, bases = stage_fasta(opts.workdir, keep)
    stage_build(opts.workdir, fasta, seqs, bases, opts)
    log("done")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
