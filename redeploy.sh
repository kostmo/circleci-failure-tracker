#!/bin/bash -xe

# This script copies credentials temporarily from a secure location
# for the duration of the "eb deploy" command.

AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json


# Allows this script to be invoked from any directory:
cd $(dirname "$0")


pushd app


./regenerate-deployable-docker-image.sh


# This generates the "Dockerrun.aws.json" file.
../run.py --prod-app --dockerrun-json-output-path $AWS_DOCKERRUN_CONFIG_FILENAME

eb deploy

rm $AWS_DOCKERRUN_CONFIG_FILENAME

popd
