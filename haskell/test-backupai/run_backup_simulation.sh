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
        ../backupai "$p" <(../inittrain4path "$p" < "$t") | ../simulation --print-interval=0 <(../inittrain4path "$p" < "$t") | ../trains2positions >> "$r"
    done
done
