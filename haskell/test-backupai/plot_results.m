#!/usr/bin/octave -qf

args = argv();

#resultFile = args{1};
#pathFile = args{2};
resultFile = "results/path_square_car.txt"
pathFile = "paths/path_square.mat"

resultData = dlmread(resultFile);
pathData = dlmread(pathFile);

# weil octave nix kann muss ich kommentarzeilen noch entfernen:
resultData = resultData(3:rows(resultData), :)

nHitches = resultData(1, 1);
nAxes = resultData(1, nHitches*2 + 2);

plot(pathData(:, 1), pathData(:, 2),
  "linestyle", ':', "color", 'k')
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

drawEveryNthTrain = floor(rows(resultData) / 5);
for i = 1:drawEveryNthTrain:rows(resultData)
  x = resultData(i, 2:2:(nHitches+1));
  y = resultData(i, 3:2:(nHitches+1));
  plot(x, y, "linestyle", '-', "color", 'y')
endfor

# "Print" current plot to file
#print("result_plot.pdf")
