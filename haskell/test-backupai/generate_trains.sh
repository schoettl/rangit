#!/bin/bash

# run it from it's directory!

mkdir -p trains/
rm trains/*

../dist/build/generatetrains/generatetrains trains/
