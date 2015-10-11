#!/bin/bash

# run it from it's directory!

mkdir -p paths/
rm paths/*.mat

../dist/build/generatepaths/generatepaths
