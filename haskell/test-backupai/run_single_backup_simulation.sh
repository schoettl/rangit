#!/bin/bash

# run it from it's directory!

usage="$0 <pathfile>:<trainfile>:<resultfile>"

# $1: error message
exitWithError() {
    echo "$1"
    exit 1
}

if [[ $# == 0 ]]; then
    exitWithError "usage: $usage"
fi

IFS=':' read -a array <<< "$1"
pathFile="${array[0]}"
trainFile="${array[1]}"
resultFile="${array[2]}"

initialTrain="$(../inittrain4path "$pathFile" < "$trainFile")"

echo "Processing: $pathFile and $trainFile" >&2

echo -e "# $pathFile\n# $trainFile" > "$resultFile"

  ../backupai "$pathFile" <(echo "$initialTrain") \
| tee "$resultFile.cmd" \
| ../simulation --print-interval=0 <(echo "$initialTrain") \
| ../trains2positions >> "$resultFile"
