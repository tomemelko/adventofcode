#!/usr/bin/env sh

stack --profile build
stack --profile exec -- adventofcode-exe $1 $2 +RTS -p
