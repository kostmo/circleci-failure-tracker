#!/bin/bash -xe



pushd ../app

./regenerate-deployable-docker-image.sh

popd



AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json


# This generates the "Dockerrun.aws.json" file.
../run.py --prod-app --entrypoint beanstalk-worker --dockerrun-json-output-path $AWS_DOCKERRUN_CONFIG_FILENAME

eb deploy
