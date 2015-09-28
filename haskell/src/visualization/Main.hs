{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)
--import System.IO
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Data.Angle
import Rangit.Train
import Rangit.Drive

-- ich will eigentlich ein handle verwenden, nämlich stdin aus System.IO
stdin :: FD -- file descriptor
stdin = 0

main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    handlerId <- inputAdd stdin [IOIn] priorityDefaultIdle (pure True)
    widgetShowAll window
    mainGUI
--    input <- getContents
--    putStr $ lines input !! 0
--    return ()
