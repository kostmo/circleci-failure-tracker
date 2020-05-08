# Setting up development environment

## Haskell dependencies

### Prerequisites
- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- [PCRE library headers](https://stackoverflow.com/a/22559967/105137)
- LLVM 6.0
- postgresql

On MacOS, these can all be installed via:
- `brew install pcre llvm@6 postgresql`
- `curl -sSL https://get.haskellstack.org/ | sh`

On Ubuntu (warning: untested):
- `sudo apt-get install libpcre3-dev llvm-6.0 postgresql`
- `curl -sSL https://get.haskellstack.org/ | sh`

### Compiling the code locally

Next, `cd` into the `app` directory of this repo, and run:

    stack build --fast

The local Haskell installation and dependencies may occupy around 4GB on disk.


## Credentials

The application makes authenticated connections with Postgres, GitHub, CircleCI, etc.

Request access to the credentials from the project owner.
Place the credentials directory as a sibling to this repo clone.


## Running the server locally

To run the Dr. CI frontend server locally using production data, execute the following from the repo root directory:

    ./run.py --prod-db

Access the server by visiting http://localhost:3001 in your browser.  Note that the scheme is **not** `https` for this local connection.
The production deployment, in contrast, requires SSL: https://dr.pytorch.org


# Deploying the application

## Docker base preparation

First, a Docker image containing all of the library dependencies must be prepared.

Execute:

    cd app
    ./prepare-docker-base-image.sh

This takes a while, but only needs to be run anytime a new third-party
dependency (i.e. a new Haskell package or package version) is introduced.

## Elastic Beanstalk deployment

### Prereqs
Make sure the `awscli` utility is installed.

### Deployment
To redeploy the frontend only:

    ./redeploy-frontend.sh

To redeploy *all* Elastic Beanstalk environments, including the frontend as well as other backend servers and Beanstalk workers, execute:

    ./redeploy-all-apps.sh

The whole process will take 5-10 minutes with a high-speed internet connection.

### Verifying deployment success

After deplyment has completed, one should peruse the latest posted PR comments, viewable from this page:
https://dr.pytorch.org/admin/comment-postings.html

Not long after the deployment, new comments should be present that are less than a few minutes old.

If the formatting/wording of the posted PR comments has changed, click through to verify that the comment as it appears on the PR is as expected.

Also, monitor the [backend performance metrics](https://github.com/kostmo/circleci-failure-tracker/tree/master/docs/operation#monitoring-performance).

## Testing

TravisCI is set up to automatically veryify that each push to GitHub can successfully compile.  Note that if one force-pushes to the repo shortly after a previous push, TravisCI may report failure on the overwritten commit.

Certain individual functions can be tested in isolation via the `./test-oneoff.sh` script inside the `app` directory.

## Debugging production services

### Downloading logs

Multiple Elastic Beanstalk workers (currently 4) service the SHA1 scanning queue.
To download all of the latest console logs for each of these workers, execute the script:

    ./tools/log-analysis/fetch_eb_worker_logs.py




