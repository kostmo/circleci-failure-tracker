#!/bin/bash

find -name "*.tix" -delete && stack run my-webapp -- --local --data-path static
