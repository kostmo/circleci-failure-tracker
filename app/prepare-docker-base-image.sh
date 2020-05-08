#!/bin/bash

# This script should be re-run anytime a new third-party
# dependency (i.e. a new Haskell package or package version) is introduced:

echo "NOTE: This command can take several minutes and requires uninterrupted network."

docker build -f Dockerfile-base --tag="karl-base" .
