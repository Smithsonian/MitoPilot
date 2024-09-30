#!/bin/bash

repo='mitopilot'
tag=$1

if [ -z "$tag" ]
then
 echo "You forgot to specify an image tag."
 exit 1
fi

# Check if already logged in by looking for credentials in Docker config
if grep -q "https://index.docker.io/v1/" ~/.docker/config.json; then
  echo "Using cached Docker Hub credentials."
  username=$(grep -oP '(?<="https://index.docker.io/v1/": {.*"auth": ")[^"]+' ~/.docker/config.json | base64 --decode | cut -d ':' -f 1)
  echo "Docker Hub username: $username"
else
  # Prompt for Docker Hub username and password if not logged in
  read -p "Enter your Docker Hub username: " username
  read -s -p "Enter your Docker Hub password: " password
  echo

  echo "Logging into Docker Hub..."
  echo "$password" | docker login -u "$username" --password-stdin
  if [ $? -ne 0 ]; then
    echo "Failed to log in to Docker Hub"
    exit 1
  fi
#fi

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
