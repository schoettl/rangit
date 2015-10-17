Testing of Backup AI
====================

This folder contains programs to run good tests on the backup AI of Rangit.

The subfolder `trains/` contains train definitions in Haskell-readable format.
The files must be `.dat` files. These files should be generated by running
`./generate_trains.sh`.

The subfolder `paths/` contains path files that contains lines of
space-separeted x/y coordinate pairs. The files must be `.txt` files. These
files should be generated by running `./generate_paths.sh`.

The subfolder `results/` contains the result files i.e. the files that are
generated by `./run_backup_simulation.sh`. The files are `.txt` files
containing rows. Each row looks like this:

    nHitches x1 y1 x2 y2 x3 y3 nAxes x1 y1 x2 y2
    ...

So `nHitches` and `nAxes` are the numbers of hitches and axes of the tested
train. The x/y coordinates after the number are the positions of these hitches
and axes. Each row represents one step of the backup. Of course `nHitches` and
`nAxis` cannot differ amongst rows.

After the result files are generated the results can be viewed:

    ./plot_results.sh results/xxx.txt

Octave pops up and shows a plot of the path and the train positions.