- [![TravisCI Build Status](https://travis-ci.com/kostmo/circleci-failure-tracker.svg?branch=master)](https://travis-ci.com/kostmo/circleci-failure-tracker)

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

### AWS dependencies

* Uses Amazon Elastic Beanstalk with Docker for hosting the webapp
* Uses a Postgres Amazon RDS database.
* Uses AWS Lambda with fixed-rate scheduled CloudWatch Events for refreshing materialized views

### AWS Elastic Beanstalk servers

* `pytorch-circle-log-scanner-dev` is a **Web Server** that hosts the Haskell backend (which makes database queries) and serves frontend HTML and Javascript
    * The code is defined in `app/webservice/src`, and the deployed binary is named `/opt/app/my-webapp` in the Docker image.
    * Its domain name is `dr.pytorch.org`
* `gh-notification-ingest-env` is a **Web Server** that services GitHub webhook notifications, storing their payloads synchronously to the database
    * The code is **also** defined in `app/webservice/src`, and the deployed binary is named `/opt/app/my-webapp` in the Docker image.
    * Its domain name is `github-notifications-ingest.pytorch.org`
* `github-notification-processor` is a **Worker** that consumes SQS messages produced by its own `cron.yaml` (this server is not well-named).
    * The code is defined in `app/eb-worker/src`, and the deployed binary is named `beanstalk-worker`.
    * It has 2 endpoints:
        * It posts to its own endpoint `/worker/scheduled-work` every 5 minutes to fetch CircleCI builds directly from the CircleCI API, both to provide information that does not exist in GitHub status notifications, and to backfill any builds of the `master` branch for which a GitHub notification was not received.
        * It posts to its own endpoint `/worker/update-pr-associations` every 2 hours. It is **intended** to update the cache of PR merge bases, but currently IS NOT IMPLEMENTED (as of 10/14/2019).
* `log-scanning-worker` is a **Worker** that consumes SQS messages produced by a Lambda function `aws-sam-getting-started-EnqueSQSBuildScansFunction-1FJXU6ZY400XC`.  It both scans build logs for the commit specified in the SQS message and posts a GitHub status, summarizing the build statuses, to that commit on GitHub.
    * The code is **also** defined in `app/eb-worker/src`, and the deployed binary is named `beanstalk-worker`.

### AWS Lambda functions

All of the Lambda functions in use are written in Python 3.7.  An experimental Lambda function namded `haskell-log-scanner` using Haskell on Docker was deployed but is not used.

* `aws-sam-getting-started-EnqueSQSBuildScansFunction-1FJXU6ZY400XC`
    * A periodic (every 3 min.) Lambda function that queries the database for builds whose logs have not yet been scanned. It creates an SQS message for each corresponding Git commit (consolidating multiple builds for the same commit into one SQS message), which will be consumed by the `log-scanning-worker` Elastic Beanstalk Worker.
* `aws-sam-getting-started-PopulateCircleCIConfigYaml-1TFF47MFKQF7U`
    * Populates CircleCI `config.yml` content to the dtabase for each `master` commit.  Checks for work every 5 min.
* `aws-sam-getting-started-HelloWorldFunction-M06818KTBNE0`
    * Refreshes a number of materialized views in the database.  It has multiple CloudWatch events scheduled at different rates for different views.


### Ingestion overview

1. A small webservice (named `gh-notification-ingest-env` in Elastic Beanstalk, and hosted at domain `github-notifications-ingest.pytorch.org`) receives GitHub webhook notifications and stores them (synchronously) in a database.
2. A periodic (3-minute interval) AWS Lambda task `EnqueSQSBuildScansFunction` queries for unprocessed notifications in the database, and enqueues an SQS message for each of them.
3. Finally, an Elastic Beanstalk Worker-tier server named `log-scanning-worker` process the SQS messages as capacity allows.

**Q. Why doesn't the webservice in the first stage (`gh-notification-ingest-env`) enqueue SQS messages directly?**

A. We want a cool-off period during which multiple builds for a given commit can be aggregated into one task for that commit.
This extra layer could be obviated if duplicate SQS messages (i.e. multiple instances of the same commit) could be consolidated while in the queue.

TODO: Enable "Content-Based Deduplication":
* https://stackoverflow.com/q/23260024/105137
* https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html


Optimizations
-------------

* We can skip inspecting *all* of the "previously-visited" builds if the master "scan" record points to the newest pattern ID.
    * Better yet, use a single DB query to get the list of out-of-date "already-visited" builds, instead of a separate query per build to obtain the unscanned pattern list.


## Features

* Periodically fetches builds directly from CircleCI API to catch up on GitHub notifications that may have been dropped

