- [![TravisCI Build Status](https://travis-ci.com/kostmo/totto.svg?branch=master)](https://travis-ci.com/kostmo/totto)

# A log analyzer for CircleCI

One would like to determine what the most common causes of intermittent build
failures/flaky tests are in the a repository so that effort can be prioritized
to fix them.

This tool obtains a list of CircleCI builds run against a GitHub repository for
a given branch, downloads their logs (stripped of ANSI escape codes) from AWS, and scans the logs for a
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


Deployment
-------------

* Uses a Postgres Amazon RDS database.
* Uses Amazon Elastic Beanstalk with Docker for hosting the webapp


Optimizations
-------------

* We can skip inspecting *all* of the "previously-visited" builds if the master "scan" record points to the newest pattern ID.
    * Better yet, use a single DB query to get the list of out-of-date "already-visited" builds, instead of a separate query per build to obtain the unscanned pattern list.


## Features

* Optionally caches downloaded logs to disk

