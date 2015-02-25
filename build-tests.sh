#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/go \
  -v `pwd`:/home/gusdev/humming \
  images.reesd.com/reesd/stack \
  sh -c 'cabal install test-framework-hunit ; ghc --make go/tests/Tests.hs'
  # TODO test-framework-hunit should be part of the stack.
