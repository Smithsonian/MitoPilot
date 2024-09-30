#!/bin/bash

repo='mitopilot'
tag=$1

if [ -z "$tag" ]
then
 echo "You forgot to specify a tag."
 exit 1
fi

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
