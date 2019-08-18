#!/bin/bash -xe

AWS_DOCKERRUN_CONFIG_FILENAME=Dockerrun.aws.json



# This generates the "Dockerrun.aws.json" file.
../run.py --prod-app --dockerrun-json-output-path $AWS_DOCKERRUN_CONFIG_FILENAME

eb deploy
