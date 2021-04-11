{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Annodate
  ( Color(..)
  , Options(..)
  , annotateLineIO
  , annotateIO
  ) where

import Control.Applicative ((<|>))
import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Text (Text, pack)
import Data.Text.IO (hGetLine, hPutStr, hPutStrLn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.IO.Handle (Handle)
import Prelude hiding (concat, putStrLn)
import System.Console.ANSI
import System.IO.Error (isEOFError)

data Options = Options
  { optsColor :: Maybe Color
  , optsNoColor :: Bool
  , optsFormat :: String
  }

type DateFormat = String

setColor :: Handle -> Maybe Color -> IO ()
setColor handle = \case
  Nothing -> hSetSGR handle [Reset]
  Just c  -> hSetSGR handle [SetColor Foreground Dull c]

annotateLineIO :: DateFormat -> Maybe Color -> Text -> Handle -> IO ()
annotateLineIO format color line output = do
  time <- getZonedTime
  setColor output color
  hPutStr output $ pack $ formatTime defaultTimeLocale format time
  setColor output Nothing
  hPutStrLn output $ ": " <> line

annotateIO :: Options -> Handle -> Handle -> IO ()
annotateIO opts input output = do
  content <- tryJust (guard . isEOFError) (hGetLine input)
  case content of
    Left  _    -> return ()
    Right line -> do
      let color = optsColor opts <|> if optsNoColor opts then Nothing else Just Magenta
      annotateLineIO (optsFormat opts) color line output
      annotateIO opts input output
