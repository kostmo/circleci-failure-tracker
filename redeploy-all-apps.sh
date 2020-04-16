#!/bin/bash -xe

# Prerequsite:
# The following command should be re-run anytime a new third-party
# dependency (i.e. a new Haskell package or package version) is introduced:
#
#     ./prepare-docker-base-image.sh


./redeploy-frontend.sh


pushd gh-notification-ingest
./redeploy.sh
popd


pushd log-scanning-worker
./redeploy.sh
popd


pushd github-notification-processor
./redeploy.sh
popd

