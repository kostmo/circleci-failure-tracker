#!/bin/bash -xe

# This script copies credentials temporarily from a secure location
# for the duration of the "eb deploy" command.

# Prerequsite:
# The following command should be re-run anytime a new third-party
# dependency is introduced:
#
#     docker build -f Dockerfile-base --tag="karl-base" .


AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json


# Disk space can fill up quickly from dangling images, so
# we clean them up every time.
docker image prune


# Allows this script to be invoked from any directory:
cd $(dirname "$0")


pushd app


./regenerate-deployable-docker-image.sh


# This generates the "Dockerrun.aws.json" file.
../run.py --prod-app --dockerrun-json-output-path $AWS_DOCKERRUN_CONFIG_FILENAME

eb deploy

rm $AWS_DOCKERRUN_CONFIG_FILENAME

popd
