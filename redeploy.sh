#!/bin/bash -xe

# This script copies credentials temporarily from a secure location,
# for the duration of the "eb deploy" command.

AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json


# Allows this script to be invoked from any directory:
cd $(dirname "$0")


pushd app


# This name is defined in stack.yaml
DOCKER_IMAGE_NAME=circleci-failure-tracker-img-small-my-webapp

DOCKER_TAG_NAME=kostmo/$DOCKER_IMAGE_NAME
stack image container --docker

docker tag $DOCKER_IMAGE_NAME $DOCKER_TAG_NAME
docker push $DOCKER_TAG_NAME

# This generates the "Dockerrun.aws.json" file.
../run.py --prod-app --dockerrun-json-output-path $AWS_DOCKERRUN_CONFIG_FILENAME

eb deploy

rm $AWS_DOCKERRUN_CONFIG_FILENAME

popd
