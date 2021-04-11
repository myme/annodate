{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Annodate
  ( Color(..)
  , Options(..)
  , annotateLineIO
  , annotateIO
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, killThread, forkIO, newMVar, MVar, readMVar, modifyMVar_)
import Control.Exception (tryJust, bracket)
import Control.Monad (guard, when)
import Data.Text (Text, pack)
import Data.Text.IO (hGetLine, hPutStr, hPutStrLn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(NoBuffering))
import Prelude hiding (getLine, concat, putStrLn)
import System.Console.ANSI
import System.IO.Error (isEOFError)

data Options = Options
  { optsColor :: Maybe Color
  , optsNoColor :: Bool
  , optsFormat :: String
  , optsCountPause :: Bool
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

inactiveTimer :: MVar Int -> Handle -> IO ()
inactiveTimer inactivity output = do
  hSetBuffering output NoBuffering
  threadDelay 1000000
  modifyMVar_ inactivity $ \i -> do
    let newVal = i + 1
    hPutStr output "\r"
    setColor output (Just White)
    hPutStr output ("Inactive for " <> pack (formatDuration newVal))
    pure newVal
  inactiveTimer inactivity output
  where formatDuration i | i == 1 = show i <> " second"
                         | otherwise = show i <> " seconds"

annotateIO :: Options -> Handle -> Handle -> IO ()
annotateIO opts input output = do
  inactivity <- newMVar 0
  content <- if optsCountPause opts
    then bracket (forkIO $ inactiveTimer inactivity output) killThread getLine
    else getLine ()
  inactiveFor <- readMVar inactivity
  when (inactiveFor > 0) $ hPutStrLn output ""
  case content of
    Left  _    -> return ()
    Right line -> do
      let color = optsColor opts <|> if optsNoColor opts then Nothing else Just Magenta
      annotateLineIO (optsFormat opts) color line output
      annotateIO opts input output
  where getLine _ = tryJust (guard . isEOFError) (hGetLine input)
