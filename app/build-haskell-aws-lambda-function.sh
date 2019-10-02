#!/bin/bash -xe

ZIP_FILENAME=function-$(date --iso-8601=seconds).zip

stack build --docker weblambda

mkdir -p build
cp `stack --docker path --local-install-root`/bin/bootstrap build

pushd build

zip $ZIP_FILENAME bootstrap && rm bootstrap

aws s3 cp $ZIP_FILENAME s3://drci-lambda-artifacts

popd

