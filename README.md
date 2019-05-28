# A log analyzer for CircleCI

One would like to determine what the most common causes of intermittent build
failures/flaky tests are in the a repository so that effort can be prioritized
to fix them.

This tool obtains a list of CircleCI builds run against a GitHub repository for
a given branch, downloads their logs from AWS, and scans the logs for a
predefined list of labeled patterns (regular expressions).

These patterns are curated by the user.  The frequency of occurrence of each
pattern are tracked and presented in a web UI.

The database tracks which builds have been already scanned for a given pattern,
so that scanning may be performed incrementally, or resumed after abort.

## Tool workflow

The tool supports a "tactical" usage model with fast iterations.

* A webhook listens for build status changes on a GitHub PR
* For each failed build, that build's log will be scanned for any of the patterns in the database tagged as "flaky"
* The tool will automatically post one more status (atop the 75 or so current statuses) that says how many of the failures were flaky.
* If all of the failures were flaky, the indicator will be green.  There will be a link in the status box to dive into the details.
* likewise for failures marked with my tool as "known problems"

# Known Problem reporting

Requiring that failures in the master branch be annotated will facilitate tracking of the frequency of "brokenness" of master over time, and allow us to measure whether this metric is improving.

It is possible for only specific jobs of a commit to be marked as "known broken", e.g. the Travis CI Lint job.

## Data flow diagram

![flow diagram](docs/data-flow.svg)

# Setup

## Prerequisites

Ubuntu packages:

    sudo apt-get install libgmp3-dev libpq-dev


Architecture notes
=============

Log storage
-----------

Console logs are stored in the Postgres database. Since large string fields (including `text` datatype)
are [automatically "TOAST"ed](https://stackoverflow.com/a/3801515/105137), this sidesteps the issue
of implementing compression.


Local testing
===========

### Without docker

To run the scanner with a fresh database:

    stack run run-scanner -- --wipe --count 10 --branch master


To run the scanner on some specific Pull Requests:

    stack run run-scanner -- --wipe --count 50 --branch pull/18339 --branch pull/18340 --branch pull/18341 --branch pull/18342 --branch pull/18343 --branch pull/18907


To launch the server, run the following from the `haskell/` directory:

    find -name "*.tix" -delete && stack run my-webapp -- --data-path static


### With docker

To test the server locally via Docker:

    docker run -p 3001:3001 -it circleci-failure-tracker-img-my-webapp


Deployment procedure
===========

Build the docker container with the following command:

    stack image container --docker


Note that we *do not* want the following in `stack.yaml`, because it breaks Intero in emacs.  The above `--docker` option takes its place.

    docker:
      enable: true

Tag the image:

    docker tag circleci-failure-tracker-img-small-my-webapp kostmo/circleci-failure-tracker-img-small-my-webapp

Push the image:

    docker push kostmo/circleci-failure-tracker-img-small-my-webapp

Redeploy webapp via `Dockerrun.aws.json`


Deployment
-------------

* Use an integrated Elastic Beanstalk database, rather than a separate RDS database.


Optimizations
-------------

* We can skip inspecting *all* of the "previously-visited" builds if the master "scan" record points to the newest pattern ID.
    * Better yet, use a single DB query to get the list of out-of-date "already-visited" builds, instead of a separate query per build to obtain the unscanned pattern list.


## Usage


### Running

1. Prepare the database

        # create a database user
        sudo -u postgres psql -c "CREATE USER logan WITH PASSWORD 'logan01';"

        # create the database
        sudo -u postgres psql < schema.sql

2. Run the scanning tool:

        app/scan.py

3. Start the webservice:

        app/frontend.py

#### Troubleshooting

If you need to start over, you can drop the database with:

    sudo -u postgres dropdb loganci

## Features

* Optionally caches downloaded logs to disk


## Development

### TODO

* Why are "Unexplained" and "Timeouts" empty?

### Capturing the database schema

Ran this command:

    ./update-database-schema.sh
