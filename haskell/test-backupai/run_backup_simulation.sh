#!/bin/bash

# run it from it's directory!

usage="$0"

# $1: error message
exitWithError() {
    echo "$1"
    exit 1
}

for p in paths/*; do
    for t in trains/*; do
        pb="${p##*/}"
        tb="${t##*/}"
        r="results/${pb%.*}_${tb%.*}.txt"
        { echo "# $p"; echo "# $t"; } > "$r"
        ../backupai "$p" "$t" | ../simulation "$t" | ../trains2positions >> "$r"
    done
done
