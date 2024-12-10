#!/bin/bash

tag="${1:-latest}"
repo="${1:-mitopilot}"

if [ -z "$tag" ]
then
 echo "You forgot to specify an image tag."
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
docker tag ${repo}:${tag} ${repo}:${tag}

echo "Local image build successfull!"
