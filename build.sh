#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/go \
  -v `pwd`:/home/gusdev/humming \
  images.reesd.com/reesd/stack \
  cabal install humming/humming.cabal --force-reinstalls
