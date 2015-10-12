
# TODO first arg
resultFile = "results/path_cube_trailer_trailer_car.txt"
# TODO second arg
pathFile = "paths/path_cube.mat"

resultData = dlmread(resultFile)
pathData = dlmread(pathFile)

nHitches = resultData(1, 1)
nAxis = resultData(1, nHitches*2 + 2)

p = plot(pathData(:, 1), pathData(:, 2))

for i = 1:nHitches
  iX = i*2
  # TODO add to plot
  plot(p, resultData(:, iX), resultData(:, iX+1))
endfor

for i = 1:nAxis
  iX = 1 + 2*nHitches + i*2
  # TODO add to plot
  plot(p, resultData(:, iX), resultData(:, iX+1))
endfor
