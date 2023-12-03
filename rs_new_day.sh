#!/usr/bin/env bash
BASEDIR=$(dirname "$0")

touch "$BASEDIR"/AoC2023/src/data/day"$1"_{easy,hard}.txt
touch "$BASEDIR"/AoC2023/src/day"$1".rs
