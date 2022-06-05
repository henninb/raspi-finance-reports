#!/bin/sh

mkdir -p src
# stack run

stack build
stack install
stack run

exit 0
