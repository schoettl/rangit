#!/bin/bash

# run it from it's directory!
# GNU parallel must be installed.

usage="$0"

for pathFile in paths/*; do
    for trainFile in trains/*; do

        pb="${pathFile##*/}"
        tb="${trainFile##*/}"
        resultFileNoExt="results/${pb%.*}_${tb%.*}"

        echo "$pathFile:$trainFile:$resultFileNoExt"
    done
done | xargs -n1 ./run_single_backup_simulation.sh
#done | parallel -j+0 --eta ./run_single_backup_simulation.sh

# GNU parallel somehow is not working parallel :(
