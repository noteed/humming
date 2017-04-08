#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`/../minicron:/home/gusdev/minicron \
  -v `pwd`:/home/gusdev/humming \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install minicron/minicron.cabal humming/humming.cabal
