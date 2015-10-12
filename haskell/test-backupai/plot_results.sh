#!/bin/bash

# run it from it's directory!

usage="$0 <result_file>"

# $1: error message
exitWithError() {
    echo "$1"
    exit 1
}

[[ -n $1 ]] || exitWithError "$usage"

file="$1"
# TODO call octave script with 2 command line arguments
octave plot_results.m "$file" "$(awk '/paths/{print $2}' "$file")"
