Prerequisites
===========

Ubuntu packages:

    sudo apt-get install libgmp3-dev libpq-dev


Local testing
===========

### Without docker

To run the scanner:

    stack run run-scanner -- --count 10 --wipe


To launch the server, run the following from the `haskell/` directory:

    find -name "*.tix" -delete && stack run my-webapp -- --data-path static


### With docker

To test the server locally via Docker:

    docker run -p 3000:3000 -it circleci-failure-tracker-img-my-webapp


Deployment procedure
===========

Build the docker container with the following command:

    stack image container --docker


Note that we *do not* want the following in `stack.yaml`, because it breaks Intero in emacs.  The above `--docker` option takes its place.

    docker:
      enable: true

TODO
================

* Allow the rescan to be kicked off on the server
    * Need a monitoring layer to prevent multiple jobs from being started.

Deployment
-------------

* Use an integrated Elastic Beanstalk database, rather than a separate RDS database.


Optimizations
-------------

* We can skip inspecting *all* of the "previously-visited" builds if the master "scan" record points to the newest pattern ID.
    * Better yet, use a single DB query to get the list of out-of-date "already-visited" builds, instead of a separate query per build to obtain the unscanned pattern list.
