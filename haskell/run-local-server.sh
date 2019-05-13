#!/bin/bash -xe

GITHUB_CLIENT_SECRET=$1

find -name "*.tix" -delete && stack run my-webapp -- --local --data-path static --github-client-secret $GITHUB_CLIENT_SECRET
