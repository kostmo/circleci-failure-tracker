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
Place the credentials directory as a sibling to the cloned Dr. CI repo directory.


## Running the server locally

To run the Dr. CI frontend server locally using production data, execute the following from the repo root directory:

    ./run.py --prod-db

Access the server by visiting http://localhost:3001 in your browser.


## Deploying the application

First, a Docker image containing all of the library dependencies must be prepared.

Execute:

    cd app
    ./prepare-docker-base-image.sh

This takes a while, but only needs to be run anytime a new third-party
dependency (i.e. a new Haskell package or package version) is introduced.

To redeploy the frontend only:

    ./redeploy-frontend.sh

To redeploy the frontend as well as all Beanstalk workers



## Testing

Certain functions can be tested in isolation via the `./test-oneoff.sh` script inside the `app` directory.




