#!/bin/bash -xe

# Pytorch hex color: #ee4c2c

IMG_DIR=../app/static/images/

convert -resize 32 -background none $IMG_DIR/pill.svg $IMG_DIR/favicon.ico
