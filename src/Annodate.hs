module Annodate
  ( annotateLine
  , annotateLineIO
  , annotateIO
  ) where

import  Control.Exception   (tryJust)
import  Control.Monad       (guard)
import  Data.Time           (FormatTime, defaultTimeLocale, formatTime)
import  Data.Time.LocalTime (getZonedTime)
import  GHC.IO.Handle       (Handle, hGetLine)
import  System.IO.Error     (isEOFError)

type DateFormat = String

annotateLine :: FormatTime t => DateFormat -> t -> String -> String
annotateLine format time line = timeString ++ ": " ++ line
    where timeString = formatTime defaultTimeLocale format time

annotateLineIO :: DateFormat -> String -> IO ()
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
