Rangit
======

General Overview
----------------

### Terms

 * *Train* refers to the chain of vehicles as a whole that is the
   car/truck/tractor with all it's trailers.
 * *Part* refers to one atomic element of the train. A part always has one fix
   axis and a left and a right hitch. (Implementation detail?)
 * *Power car* refers to the driving vehicle in the train. There can only be
   one power car per train and it is always rightmost in the list of parts.
 * *To backup train* means driving the train backwards only e.g. to park a
   trailer or the train in a certain position or to drive back on the street.
 * *Path* refers to the route that should be followed when backing up the
   train. (Better term: *Route*?)
 * *Left* and *right*: The train is actually a list of parts. The right-most
   (i.e. last) element in the list is always the power car. The front of the
   power car is therefore the right-most point of the train (usually the power
   car's right hitch). Therefore, *left hitch* is the rear and *right hitch* is
   the front hitch of a part.

### Description

See this [Google Document](https://docs.google.com/document/d/1CzPtPmdwdxAnVIlMyI-MKNdvQRLbaiZlK0k8D2TotXQ/edit?usp=sharing).

### Limitations

 1. A train can only have one steer. For example the following trains cannot be modeled:

     * Amerikanisches Drehleiterfahrzeug der Feuerwehr (wenn es ganz hinten
       eine zweite Lenkachse hat)
     * Ein Fahrzeug das per Abschleppstange abgeschleppt wird (ein kleiner
       Truck mit Deichsel, wie ihn manche amerikanischen Wohnmobile
       hinterherziehen, ist dagegen in Ordnung)

 2. No part can have it's right hitch left of it's left hitch.

 3. Only the power car can have it's right hitch left of it's axis. In this
    case the steer axis is left of the fix axis.

API
---

### Preface

 * All lengths are in meters
 * All angles are in radians

### Backup API

    backupTrain ( path : String, train : String ) -> driveCommands : String

 * `path` contains lines with x/y pairs of coordinates. It has the following form:


```
0 0
1 0
2 1
. .
. .
. .
```

 * `train` is a JSON-String describing the train.

 * `driveCommands` contains lines with length and angle pairs. Each line specify how for to drive with a certain steer angle. Length must be negative for backup. `driveCommands` has the following form:

```
-0.1 1.2
-0.6 0.8
-1 0.2
-2 0
. .
. .
. .
```

### Simulation API

    driveTrain ( train : String, length : Double, angle : Double ) -> newTrain : String

 * `train` is a JSON-String describing the car and trailers.
 * `length` is the length to drive.
 * `angle` is the steer angle to drive.
 * `newTrain` is a JSON-String describing the driven car and trailers.
