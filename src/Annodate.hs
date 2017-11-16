{-# LANGUAGE OverloadedStrings #-}

module Annodate
  ( annotateLine
  , annotateLineIO
  , annotateIO
  ) where

import           Control.Exception   (tryJust)
import           Control.Monad       (guard)
import           Data.Text           (Text, pack)
import qualified Data.Text           as T
import           Data.Text.IO        (hGetLine, putStrLn)
import           Data.Time           (FormatTime, defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (getZonedTime)
import           GHC.IO.Handle       (Handle)
import           Prelude             hiding (concat, putStrLn)
import           System.IO.Error     (isEOFError)

type DateFormat = String

annotateLine :: FormatTime t => DateFormat -> t -> Text -> Text
annotateLine format time line = T.concat [timeString, ": ", line]
    where timeString = pack $ formatTime defaultTimeLocale format time

annotateLineIO :: DateFormat -> Text -> IO ()
annotateLineIO format line = do
            time <- getZonedTime
            putStrLn $ annotateLine format time line

annotateIO :: DateFormat -> Handle -> IO ()
annotateIO format handle = do
    input <- tryJust (guard . isEOFError) (hGetLine handle)
    case input of
        Left  _    -> return ()
        Right line -> do
          annotateLineIO format line
          annotateIO format handle
