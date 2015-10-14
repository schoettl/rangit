#!/bin/bash

# run it from it's directory!

mkdir -p paths/
rm paths/*

../dist/build/generatepaths/generatepaths paths/
