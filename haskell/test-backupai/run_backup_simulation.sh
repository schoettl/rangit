#!/bin/bash

# run it from it's directory!

usage="$0"

# $1: error message
exitWithError() {
    echo "$1"
    exit 1
}

for pathFile in paths/*; do
    for trainFile in trains/*; do
        pb="${pathFile##*/}"
        tb="${trainFile##*/}"
        resultFile="results/${pb%.*}_${tb%.*}.txt"
        initialTrain="$(../inittrain4path "$pathFile" < "$trainFile")"

        { echo "# $pathFile"; echo "# $trainFile"; } > "$resultFile"

          ../backupai "$pathFile" <(echo "$initialTrain") \
        | ../simulation --print-interval=0 <(echo "$initialTrain") \
        | ../trains2positions >> "$resultFile"
    done
done
