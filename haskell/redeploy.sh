#!/bin/bash

# This name is defined in stack.yaml
DOCKER_IMAGE_NAME=circleci-failure-tracker-img-small-my-webapp

DOCKER_TAG_NAME=kostmo/$DOCKER_IMAGE_NAME
stack image container --docker

docker tag $DOCKER_IMAGE_NAME $DOCKER_TAG_NAME
docker push $DOCKER_TAG_NAME

eb deploy
