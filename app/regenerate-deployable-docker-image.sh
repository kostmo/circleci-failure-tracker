#!/bin/bash -xe


# This name is defined in stack.yaml
DOCKER_IMAGE_NAME=circleci-failure-tracker-img-small-my-webapp

DOCKER_TAG_NAME=kostmo/$DOCKER_IMAGE_NAME

time docker build -f Dockerfile-deploy --tag="$DOCKER_TAG_NAME" .

docker push $DOCKER_TAG_NAME
