# Preparing a new MitoPilot container

Maintainer notes. Everything here is run by hand from the repository root.

The image tag must match `Version:` in `DESCRIPTION` exactly. MitoPilot builds
the container name from the installed package version
(`paste0("macguigand/mitopilot:", packageVersion("MitoPilot"))`), so a mismatch
means new projects point at a tag that does not exist.

Two independent artifacts get staged into `docker/` before a build. Neither is
tracked in git (see `.gitignore` lines 19 and 22):

| File | Made by | Rebuild cadence |
|---|---|---|
| `docker/MitoPilot_<version>.tar.gz` | `devtools::build()`, run by the deploy script | every release |
| `docker/mito_metazoa_blastdb.tar.gz` | the two Python tools below | roughly quarterly |

Most releases only need part 2.

---

## 1. Rebuild the local BLAST database

Only needed when you want fresher GenBank coverage. Takes a few hours, mostly
NCBI download, and about 6 GB of scratch space. If you are skipping this, keep
the existing `docker/mito_metazoa_blastdb.tar.gz` in place and jump to part 2.

Pick one working directory and use it for both scripts. The two scripts ship
with *different* defaults, so always pass `--workdir` explicitly.

```bash
export NCBI_API_KEY=...                      # optional, lifts the rate limit 3/s -> 10/s
WD=~/mitopilot_blastdb_build

# Stage 1: harvest, filter, fetch, makeblastdb
python3 tools/build_local_blast_db.py --workdir "$WD"

# Stage 2: collapse byte-identical RefSeq/GenBank duplicate genomes
python3 tools/dedup_local_blast_db.py --workdir "$WD"

# Stage the result for the image build
cp "$WD/mito_metazoa_blastdb.tar.gz" docker/
```

Notes:

- The build script needs `makeblastdb`. The image ships BLAST+ 2.16 or newer,
  so match that. If `makeblastdb` is not on your `PATH` the script falls back
  to Docker, using the image named by
  `MITOPILOT_IMAGE`, which defaults to a local-only tag. Set it to a published
  one first: `export MITOPILOT_IMAGE=macguigand/mitopilot:1.5.2`.
- Progress is written to `$WD/status.txt` if you want to watch a long run.
- To continue an interrupted run, add `--resume`. Without it the script clears
  the working directory and starts over. Only use `--resume` when none of the
  query or filter options have changed, since the resumed run reuses the old
  cached data but stamps the new options into `VERSION`.
- Do not skip the dedup step. It removes about 37,000 redundant genomes and is
  what keeps the right accession at rank 1 in the WF1 reference search. Both
  scripts write the same output filename, so a skipped dedup is invisible later.

Check the result before building an image:

```bash
cat "$WD/db/VERSION"
```

`deduplicated_identical` must read `True`, and `built` should be today.

---

## 2. Rebuild and push the image

```bash
# 1. Bump the version
#    edit DESCRIPTION -> Version: 1.5.3

# 2. Build and push, using the SAME string as DESCRIPTION
./docker/deploy-dockerhub.sh 1.5.3
```

The script deletes any old package tarballs from `docker/`, rebuilds the
documentation, builds the new package tarball, builds the image, prompts for
your Docker Hub credentials, and pushes `macguigand/mitopilot:1.5.3`. Expect 30 to 60 minutes on a cold cache;
the BLAST database layer alone is 835 MiB unpacked.

Two other targets exist and take the same argument:

- `./docker/deploy-local.sh` builds `mitopilot:latest` without pushing, for
  testing. Run it with **no argument**: passing a tag currently names the image
  after the tag twice.
- `./docker/deploy-aws.sh 1.5.3` pushes to the NMNH ECR registry.

### Verify before announcing the release

```bash
docker run --rm --entrypoint grep macguigand/mitopilot:1.5.3 \
  '^Version:' /opt/conda/lib/R/library/MitoPilot/DESCRIPTION
```

This must print the tag you just pushed. It is the one check worth doing every
time, because a stale package tarball in the build context produces a working
image that quietly contains the previous release.

Then run the test project against the new tag end to end (assemble, annotate,
export) before pointing users at it.

---

## Gotchas

- **Never leave more than one package tarball in `docker/`.**
  `docker/Dockerfile` copies `docker/MitoPilot_*.tar.gz` with a wildcard, and
  when more than one matches the build silently picks the last one
  alphabetically rather than the newest, so `1.4.9` beats `1.4.10`. This shipped
  twice before the deploy scripts started clearing them (`1.4.10` contains
  1.4.9; `1.4.12` contains 1.4.11). The scripts now `rm -f
  docker/MitoPilot_*.tar.gz` themselves, so this only bites if you stage a
  tarball by hand or build the image without them.
- **The deploy scripts do not stop on a failed R build.** If
  `devtools::document()` or `devtools::build()` errors, the script keeps going
  into `docker build`. Since the old tarball has already been deleted by then,
  the `COPY` now fails loudly instead of shipping the previous release, but the
  R error itself scrolls past. Watch the R output.
- **Do not rename the database.** `mito_metazoa` is hardcoded on the Nextflow
  side; the tarball must stay `docker/mito_metazoa_blastdb.tar.gz`.
- **Running dedup twice in the same working directory fails** at the final
  directory swap. The tarball it writes is still valid. Clean up with
  `rm -rf "$WD/db_prededup"` and rerun, or start from a fresh build.
