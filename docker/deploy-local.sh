#!/bin/bash

tag="${1:-latest}"
repo="${2:-mitopilot}"

if [ -z "$tag" ]
then
 echo "You forgot to specify an image tag."
 exit 1
fi

# Local BLAST database, staged into the build context and not tracked in git
if [ ! -s docker/mito_metazoa_blastdb.tar.gz ]; then
  echo "Missing docker/mito_metazoa_blastdb.tar.gz (about 275 MiB)."
  echo "Build it with tools/build_local_blast_db.py then tools/dedup_local_blast_db.py,"
  echo "and copy the resulting mito_metazoa_blastdb.tar.gz into docker/."
  exit 1
fi

# Build R pkg for install
# Stale tarballs must go first: the Dockerfile COPY glob picks the last match
# alphabetically, so a leftover 1.4.9 would beat a new 1.4.10.
rm -f docker/MitoPilot_*.tar.gz
Rscript -e 'devtools::document()'
Rscript -e 'devtools::build(path="docker", vignettes = FALSE)'

# Build image
docker build -f docker/Dockerfile --progress=plain -t ${repo}:${tag} .
if [ $? -ne 0 ]; then
  echo "Failed to build the Docker image"
  exit 1
fi
docker tag ${repo}:${tag} macguigand/mitopilot:${tag}

echo "Local image build successfull!"
