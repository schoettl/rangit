{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.IORef
import Data.Reference
import Data.Angle
import Rangit.Train
import Rangit.Drive
import Debug.Trace

-- | File descriptor: 0 = stdin
stdin :: FD 
stdin = 0

main :: IO ()
main = do
    ref <- newRef [] :: IO (IORef [Part])

    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    handlerId <- inputAdd stdin [IOIn] priorityDefaultIdle (handleInput ref)

    canvas <- drawingAreaNew
    --canvas `on` draw $ drawParts ref

    widgetShowAll window
    mainGUI

-- | Handle input i.e. read the positions and draw the parts.
handleInput :: IORef [Part] -> IO Bool
handleInput ref = do
    input <- getLine
    let parts = read input :: [Part]
    writeRef ref parts
    --widgetQueueDraw window -- send redraw request
    putStrLn input
    return True

drawParts :: IORef [Part] -> Render ()
drawParts ref = do
    parts <- liftIO $ readRef ref

    trace (show parts) return ()

    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath
    
    stroke
