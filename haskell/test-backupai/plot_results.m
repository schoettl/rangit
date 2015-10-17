#!/usr/bin/octave -qf

# without --persist, octave closes the plot window immediately
# but there is no short form of this option and there must be only one argument in the shebang line

args = argv();

resultFile = args{1};
pathFile = args{2};

resultData = dlmread(resultFile);
pathData = dlmread(pathFile);

# weil octave nix kann muss noch ich zwei kommentarzeilen oben entfernen:
resultData = resultData(3:rows(resultData), :);

nHitches = resultData(1, 1);
nAxes = resultData(1, nHitches*2 + 2);

plot(pathData(:, 1), pathData(:, 2),
  "linestyle", ':', "marker", '+', "color", 'k')

# only show this rect [x_min, x_max, y_min, y_max]
#axis([0, 2, 0, 4])

hold on;

for i = 1:nHitches
  iX = i*2;
  plot(resultData(:, iX), resultData(:, iX+1),
    "linestyle", ':', "color", 'b')
endfor

for i = 1:nAxes
  iX = 1 + 2*nHitches + i*2;
  plot(resultData(:, iX), resultData(:, iX+1),
    "linestyle", ':', "color", 'g')
endfor

nTrainToDraw = 5;
drawEveryNthTrain = floor(rows(resultData) / nTrainToDraw);

for i = 1:drawEveryNthTrain:rows(resultData)

  iHitchesEnd = 2*nHitches + 1;
  x = resultData(i, 2:2:iHitchesEnd);
  y = resultData(i, 3:2:iHitchesEnd);
  plot(x, y,
    "linestyle", '-',
    "linewidth", 2,
    "marker" , 'o', # bug in doc: marker instead of markerstyle
    "color", 'c')

  iAxesStart = 2*nHitches + 2;
  iAxesEnd = iAxesStart + 2*nAxes;
  x = resultData(i, (iAxesStart+1):2:iAxesEnd);
  y = resultData(i, (iAxesStart+2):2:iAxesEnd);
  plot(x, y,
    "linestyle", '-',
    "linewidth", 3,
    "marker" , '*', # bug in doc: marker instead of markerstyle
    "color", 'y')
endfor
