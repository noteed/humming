#! /usr/bin/env bash

docker run \
  -v `pwd`/../minicron:/home/gusdev/minicron \
  -v `pwd`:/home/gusdev/humming \
  images.reesd.com/reesd/stack \
  cabal install minicron/minicron.cabal humming/humming.cabal
