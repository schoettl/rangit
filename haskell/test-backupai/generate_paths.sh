#!/bin/bash

# run it from it's directory!

usage="$0 <xstep>"

mkdir -p paths/
rm -f paths/*

../dist/build/generatepaths/generatepaths -x "$1" paths/ &>/dev/null || echo "usage: $usage" >&2
