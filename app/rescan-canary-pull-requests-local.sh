#!/bin/bash -xe

time stack run run-scanner -- --count 1000 --branch pull/18339 --branch pull/18340 --branch pull/18341 --branch pull/18342 --branch pull/18343 --branch pull/18907 2>&1 | tee mylog.txt
