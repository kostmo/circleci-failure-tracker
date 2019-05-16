#!/bin/bash -xe

# This script copies credentials temporarily from a secure location,
# for the duration of the "eb deploy" command.

AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json
AWS_DOCKERRUN_CONFIG_SOURCE_PATH=../../circleci-failure-tracker-credentials/$AWS_DOCKERRUN_CONFIG_FILENAME

# This name is defined in stack.yaml
DOCKER_IMAGE_NAME=circleci-failure-tracker-img-small-my-webapp

DOCKER_TAG_NAME=kostmo/$DOCKER_IMAGE_NAME
stack image container --docker

docker tag $DOCKER_IMAGE_NAME $DOCKER_TAG_NAME
docker push $DOCKER_TAG_NAME

cp $AWS_DOCKERRUN_CONFIG_SOURCE_PATH .

eb deploy

rm $AWS_DOCKERRUN_CONFIG_FILENAME
