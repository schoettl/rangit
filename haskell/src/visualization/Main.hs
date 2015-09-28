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
import Control.Monad

-- | File descriptor: 0 = stdin
stdin :: FD 
stdin = 0

-- | Redraw interval in milliseconds.
redrawInterval = 1000

main :: IO ()
main = do
    ref <- newRef [] :: IO (IORef [[Part]])

    initGUI
    window <- windowNew
    set window [ windowTitle := "Rangit Visualization" ]

    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas

    window `on` deleteEvent $ liftIO mainQuit >> return False

    --handlerId <- inputAdd stdin [IOIn] priorityDefault (handleInput ref)
    handleInput ref

    timeoutAdd (timeoutHandler window) redrawInterval

    canvas `on` draw $ drawParts ref

    widgetShowAll window
    mainGUI

-- | Handle input i.e. read the positions and draw the parts.
handleInput :: IORef [[Part]] -> IO Bool
handleInput ref = do
    input <- getContents
    let partsList = map (read :: String -> [Part]) $ lines input
    writeRef ref partsList
    --widgetQueueDraw window -- send redraw request
    mapM print partsList
    return True

drawParts :: IORef [[Part]] -> Render ()
drawParts ref = do
    partsList <- liftIO $ readRef ref
    when ((not . null) partsList) $ do
        let parts:rest = partsList
        liftIO $ writeRef ref rest

        trace (show parts) return ()

        setSourceRGB 1 1 0
        setLineWidth 5

        moveTo 120 60
        lineTo 60 110
        lineTo 180 110
        closePath
        
        stroke

timeoutHandler :: Window -> IO Bool
timeoutHandler window = do
    widgetQueueDraw window -- send redraw request
    putStrLn "timeout handler"
    return True
