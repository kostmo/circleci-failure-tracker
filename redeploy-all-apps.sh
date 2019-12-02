#!/bin/bash -xe

./redeploy.sh


pushd log-scanning-worker
./redeploy.sh
popd


pushd github-notification-processor
./redeploy.sh
popd


pushd gh-notification-ingest
./redeploy.sh
popd
