{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.IORef
import Data.Reference
import Rangit.Train
import Rangit.Drive
import Control.Monad
import System.Console.Docopt

-- | File descriptor: 0 = stdin
stdin :: FD
stdin = 0

-- | Redraw interval in milliseconds.
redrawInterval = 2000

-- | Interval to read one line from input in milliseconds.
inputReadInterval = 100

main :: IO ()
main = do
    list <- readInputBlocking
    ref <- newRef list :: IO (IORef [Train])

    initGUI
    window <- windowNew
    set window [ windowTitle := "Rangit Visualization" ]

    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas

    window `on` deleteEvent $ liftIO mainQuit >> return False

    -- this won't work because getLine and getContents are blocking: as long as there is no EOF the GUI will hang (sooner or later)
    --inputAdd stdin [IOIn] priorityDefault (readInputLinewise ref)

    -- this won't work because getLine is blocking: if there is no new line and also no EOF then the GUI will hang
    --timeoutAdd readInputLinewise inputReadInterval

    -- another possibility for files only (not stdin) could be a tailf implementation like: https://gist.github.com/radix/e9b90c09b75fbe945d69

    timeoutAdd (initiateRedraw window) redrawInterval

    canvas `on` draw $ drawVisualization ref

    widgetShowAll window
    mainGUI

-- | Read input linewise.
-- When this program is used in a pipe and this function is registerd with inputAdd and called by GTK main loop
-- it is called once for each change in input stream. This means that for `vis <data` just one line is read and
-- for `(cat data; cat -) | vis` a second line is read when the user inputs a new line.
readInputLinewise :: IORef [Train] -> IO Bool
readInputLinewise ref = do
    input <- getLine
    let parts = read input :: Train
    list <- liftIO $ readRef ref
    let newList = list ++ [parts]
    writeRef ref newList
    putStrLn input
    return True

-- | Read full input blocking.
-- This can be used if all input is already there in a file.
readInputBlocking :: IO [Train]
readInputBlocking = do
    input <- getContents
    let list = map (read :: String -> Train) $ lines input
    mapM_ print list
    return list

drawVisualization :: IORef [Train] -> Render ()
drawVisualization ref = do
    list <- liftIO $ readRef ref
    newList <- processList list
    liftIO $ writeRef ref newList

-- | Process list of trains i.e. draw current one and return rest of list.
processList :: [Train] -> Render [Train]
processList []        = return []
processList [ps]      = drawParts ps >> return [ps]
processList (ps:rest) = drawParts ps >> return rest

drawParts :: Train -> Render ()
drawParts ps = mapM_ (drawPart ps) ps

drawPart :: Train -> Part -> Render ()
drawPart ps p = do
    setSourceRGB 0 1 1
    setLineWidth 5
    moveTo `callWithPosition` partPosition p
    lineTo `callWithPosition` calculateCenterPosition p
    lineTo `callWithPosition` calculateLeftHitchPosition p
    stroke
    let wheelbase = calculateNiceWheelbase ps
    drawAxis wheelbase p

drawAxis :: Double -> Part -> Render ()
drawAxis wheelbase p = do
    setSourceRGB 1 1 1
    setLineWidth 2
    --newPath
    let (p1, p2) = calculatePerpendicularLine (calculateCenterPosition p) wheelbase (partAngle p)
    moveTo `callWithPosition` p1
    lineTo `callWithPosition` p2
    stroke

-- | Send redraw request.
initiateRedraw :: Window -> IO Bool
initiateRedraw window = widgetQueueDraw window >> return True

callWithPosition :: (Double -> Double -> Render ()) -> Position -> Render ()
callWithPosition f p = uncurry f $ (scaleAndOffset . positionToPair) p

positionToPair :: Position -> (Double, Double)
positionToPair (Vector2 x y) = (x, y)

scaleAndOffset :: (Double, Double) -> (Double, Double)
scaleAndOffset (x, y) = let factor = 10 in (factor*x + 200, factor*y + 200)

calculateNiceWheelbase :: Train -> Double
calculateNiceWheelbase ps = 0.5 * partLength (last ps)

calculatePerpendicularLine :: Position -> Double -> Double -> (Position, Position)
calculatePerpendicularLine centerPoint lineLength angle = (calcPoint (+), calcPoint (-)) where
    halfLength = lineLength / 2
    calcPoint addOrSubtr = calculatePositionByPointAngleLength centerPoint (angle `addOrSubtr` (pi/2)) halfLength
