#!/bin/bash -xe

# Prerequsistes:
# Install the "sam" tool from AWS

cd refresh-grid-view

./generate_credentials_module.py


sam build

# The following command will transform the checked-in file "template.yaml" into a
# build artifact named "packaged.yaml".
sam package --output-template packaged.yaml --s3-bucket drci-lambda-artifacts

sam deploy --template-file packaged.yaml --region us-east-2 --capabilities CAPABILITY_IAM --stack-name aws-sam-getting-started

