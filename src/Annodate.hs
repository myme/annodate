module Annodate
  ( annotateLine
  , annotateIO
  ) where

import  Control.Exception (tryJust)
import  Control.Monad     (guard)
import  Data.Time         (FormatTime, defaultTimeLocale, formatTime, getCurrentTime)
import  GHC.IO.Handle
import  System.IO.Error   (isEOFError)

type DateFormat = String

annotateLine :: FormatTime t => DateFormat -> t -> String -> String
annotateLine format time line = timeString ++ ": " ++ line
    where timeString = formatTime defaultTimeLocale format time

annotateIO :: DateFormat -> Handle -> IO ()
annotateIO format handle = do
    input <- tryJust (guard . isEOFError) (hGetLine handle)
    case input of
        Left  _    -> return ()
        Right line -> do
            time <- getCurrentTime
            putStrLn $ annotateLine format time line
            annotateIO format handle
