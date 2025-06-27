#!/bin/bash

repo='mitopilot'
#tag="${1:-latest}"
tag="devel"

if [ -z "$tag" ]
then
 echo "You forgot to specify an image tag."
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
