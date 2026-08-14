#!/bin/bash

repo='mitopilot'
tag=$1

if [ -z "$tag" ]
then
 echo "You forgot to specify a tag."
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
Rscript -e 'devtools::document()'
Rscript -e 'devtools::build(path="docker", vignettes = FALSE)'

# Build image
docker build -f docker/Dockerfile --progress=plain -t ${repo}:${tag} .
if [ $? -ne 0 ]; then
  echo "Failed to build the Docker image"
  exit 1
fi

# Tag image
docker tag ${repo}:${tag} 220571360826.dkr.ecr.us-east-1.amazonaws.com/${repo}:${tag}
if [ $? -ne 0 ]; then
  echo "Failed to tag the Docker image"
  exit 1
fi

# Push to ECR
aws ecr get-login-password | docker login --username AWS --password-stdin 220571360826.dkr.ecr.us-east-1.amazonaws.com
docker push 220571360826.dkr.ecr.us-east-1.amazonaws.com/${repo}:${tag}
if [ $? -ne 0 ]; then
  echo "Failed to push the Docker image to AWS ECR"
  exit 1
fi

echo "Image pushed successfully!"
