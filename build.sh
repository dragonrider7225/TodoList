#!/bin/bash

./configure.sh "$1" && cabal build

rm src/IODiscrete.hs
