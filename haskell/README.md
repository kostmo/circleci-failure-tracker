Prerequisites
===========

    sudo apt-get install libgmp3-dev libpq-dev


Local testing
===========

### Without docker

To run the scanner:

    stack run run-scanner -- --count 10 --wipe


To launch the server, run the following from the `haskell/` directory:

    find -name "*.tix" -delete && stack run my-webapp -- --data-path static


Deployment procedure
===========

Build the docker container with the following command:

    stack image container --docker


Note that we *do not* want the following in `stack.yaml`, because it breaks Intero in emacs.  The above `--docker` option takes its place.

    docker:
      enable: true

To test the server locally via Docker:

    docker run -p 3000:3000 -it circleci-failure-tracker-img-my-webapp


