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
 * *To back up train* means driving the train backwards only e.g. to park a
   trailer or the train in a certain position or to drive back on the street.
 * *Path* refers to the route that should be followed when backing up the
   train.
 * *Left* and *right*: The train is actually a list of parts. The right-most
   (i.e. last) element in the list is always the power car. The front of the
   power car is therefore the right-most point of the train (usually the power
   car's right hitch). Therefore, *left hitch* is the rear and *right hitch* is
   the front hitch of a part. (Maybe change naming; left to rear/back and right
   to front)

### Description

See this [Google Document](https://docs.google.com/document/d/1CzPtPmdwdxAnVIlMyI-MKNdvQRLbaiZlK0k8D2TotXQ/edit?usp=sharing).

### The Model

My friend Korbinian came up with a very cool model that we are using for the
simulation and the backup AI. It is very simple in concept and easy to
implement but powerful enough to model almost all realistic trains.

We are modeling an arbitrary train as a list of parts. A part (as defined in
src/library/Rangit/Train.hs) has exactly one axis. From the center of this axis
the distances to the left and right hitches are defined. The position of a part
is defined by the position of the right hitch and the angle of the part, more
specifically the angle between the horizontal line (West-East-line) and the
line from the left to the right hitch, counter-clockwise.

Here is an introduction how this model works:

 1. To model just one car we use one part. For power cars there is this special
    case: The axis of the part corresponds to the rear axis of the car. The
    right hitch corresponds to the front (steer) axis of the car.  Remember we
    don't need a right hitch (see Terms) so instead of the right hitch we
    assume the steer axis.

 2. To model a car with a simple trailer we just add one part for the trailer.

 3. To model a 2-axes trailer we add two parts. It's important that the front
    part of the trailer has it's left hitch right on it's axis center i.e.
    the distance to the left hitch is zero.  This way the rear part of the
    trailer docks with it's right hitch right on the axis center of the front
    part.

To move the train (in the simulation) we move parts starting with the power
car. A part is moved as following:

 1. Turn the part towards the target around it's axis center.
 2. Move the part straight to the target.
 3. The target for the next part will be the current part's left hitch.

This algorithm of course only brings satisfying results if the target is very
near. Therefore to executing a drive command (distance to drive and steer
angle) is done in very small steps e.g. centimiters.

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

 * `driveCommands` contains lines with length and angle pairs. Each line
   specify how for to drive with a certain steer angle. Length must be negative
   for backup. `driveCommands` has the following form:

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
