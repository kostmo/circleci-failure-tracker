# Setting up development environment

## Haskell dependencies

### OS preqrequisites

One must have the PCRE library headers installed. See https://stackoverflow.com/a/22559967/105137


### Compiling the code locally
First [install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) to bootstrap your Haskell dev environment.

Next, `cd` into the `app` directory of this repo, and run:

    stack build --fast

The local Haskell installation and dependencies may occupy around 4GB on disk.


## Credentials

The application makes authenticated connections with Postgres, GitHub, CircleCI, etc.
Place the credentials directory as a sibling to this repo clone.


## Running the server locally

To run the Dr. CI frontend server locally using production data, execute the following from the repo root directory:

    ./run.py --prod-db

Access the server by visiting http://localhost:3001 in your browser.  Note that the scheme is **not** `https` for this local connection.
The production deployment, in contrast, requires SSL: https://dr.pytorch.org


## Deploying the application

### Docker base preparation

First, a Docker image containing all of the library dependencies must be prepared.

Execute:

    cd app
    ./prepare-docker-base-image.sh

This takes a while, but only needs to be run anytime a new third-party
dependency (i.e. a new Haskell package or package version) is introduced.

### Elastic Beanstalk deploment

#### Prereqs
Make sure the `awscli` utility is installed.

#### Deployment
To redeploy the frontend only:

    ./redeploy-frontend.sh

To redeploy *all* Elastic Beanstalk environments, including the frontend as well as other backend servers and Beanstalk workers, execute:

    ./redeploy-all-apps.sh

The whole process will take 5-10 minutes with a high-speed internet connection.


## Testing

TravisCI is set up to automatically veryify that each push to GitHub can successfully compile.  Note that if one force-pushes to the repo shortly after a previous push, TravisCI may report failure on the overwritten commit.

Certain individual functions can be tested in isolation via the `./test-oneoff.sh` script inside the `app` directory.




