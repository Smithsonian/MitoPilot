#!/bin/bash

repo='mitopilot'
tag="${1}"
#tag="devel"

if [ -z "$tag" ]
then
 echo "You forgot to specify an image tag."
 exit 1
fi

# Local BLAST database, staged into the build context and not tracked in git.
# Checked before the credential prompt so a missing artifact fails immediately.
if [ ! -s docker/mito_metazoa_blastdb.tar.gz ]; then
  echo "Missing docker/mito_metazoa_blastdb.tar.gz (about 275 MiB)."
  echo "Build it with tools/build_local_blast_db.py then tools/dedup_local_blast_db.py,"
  echo "and copy the resulting mito_metazoa_blastdb.tar.gz into docker/."
  exit 1
fi

# Prompt for Docker Hub username and password
read -p "Enter your Docker Hub username: " username
read -s -p "Enter your Docker Hub password: " password

echo "Logging into Docker Hub..."
echo "$password" | docker login -u "$username" --password-stdin
if [ $? -ne 0 ]; then
  echo "Failed to log in to Docker Hub"
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

# Tag image
docker tag ${repo}:${tag} ${username}/${repo}:${tag}
if [ $? -ne 0 ]; then
  echo "Failed to tag the Docker image"
  exit 1
fi

# Push the Docker image to Docker Hub
docker push ${username}/${repo}:${tag}
if [ $? -ne 0 ]; then
  echo "Failed to push the Docker image to Docker Hub"
  exit 1
fi

echo "Image pushed successfully!"
