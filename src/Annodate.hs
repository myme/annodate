module Annodate
  ( Color (..),
    Options (..),
    annotateIO,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, modifyMVar_, newMVar, threadDelay, withMVar)
import Control.Exception (bracket, bracket_, tryJust)
import Control.Monad (forever, guard, when)
import Data.Text (Text, pack)
import Data.Text.IO (hGetLine, hPutStr, hPutStrLn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), Handle, hIsTerminalDevice, hSetBuffering)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (Dull),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    hSetSGR,
  )
import System.IO.Error (isEOFError)
import Prelude hiding (concat, getLine, putStrLn)

data Options = Options
  { optsColor :: !(Maybe Color),
    optsNoColor :: !Bool,
    optsFormat :: !String,
    optsPauseThreshold :: !Int,
    optsNoCountPause :: !Bool
  }

type DateFormat = String

setColor :: Handle -> Maybe Color -> IO ()
setColor handle = \case
  Nothing -> hSetSGR handle [Reset]
  Just c -> hSetSGR handle [SetColor Foreground Dull c]

printColored :: Handle -> Maybe Color -> Text -> IO ()
printColored handle color text = case color of
    Nothing -> hPutStr handle text
    Just c -> do
      setColor handle (Just c)
      hPutStr handle text
      setColor handle Nothing

printAnnotatedLine :: DateFormat -> Maybe Color -> Text -> Handle -> IO ()
printAnnotatedLine format color line output = do
  time <- getZonedTime
  printColored output color (pack $ formatTime defaultTimeLocale format time)
  hPutStrLn output line

withInactivity :: Int -> (Int -> IO ()) -> (Int -> IO ()) -> IO a -> IO a
withInactivity threshold onTick onDone action = do
  inactivity <- newMVar 0

  let callbackIf f x = when (x >= threshold) $ f x
  let inactiveTimer = forkIO $
        forever $ do
          threadDelay 1000000
          modifyMVar_ inactivity $ \i -> do
            let newVal = i + 1
            callbackIf onTick newVal
            pure newVal

  bracket inactiveTimer killThread $ \_ -> do
    result <- action
    withMVar inactivity $ callbackIf onDone
    pure result

inactivityMessage :: Int -> Text
inactivityMessage duration = "Inactive for " <> pack (formatDuration duration)
  where
    formatDuration i
      | i == 1 = show i <> " second"
      | otherwise = show i <> " seconds"

annotateIO :: Options -> Handle -> Handle -> IO ()
annotateIO opts input output = do
  let getLine = tryJust (guard . isEOFError) (hGetLine input)

  isTTY <- hIsTerminalDevice output
  content <-
    if optsNoCountPause opts
      then getLine
      else
        if not isTTY
          then do
            let onInactive = const $ pure ()
                onDone = hPutStrLn output . inactivityMessage
            withInactivity (optsPauseThreshold opts) onInactive onDone getLine
          else do
            let setBuffering = hSetBuffering output
            let onInactive duration = bracket_ (setBuffering NoBuffering) (setBuffering LineBuffering) $ do
                  hPutStr output "\r"
                  setColor output (Just White)
                  hPutStr output (inactivityMessage duration)
                  setColor output Nothing
                onDone = const $ hPutStrLn output ""
            withInactivity (optsPauseThreshold opts) onInactive onDone getLine

  case content of
    Left _ -> return ()
    Right line -> do
      let color = optsColor opts <|> if optsNoColor opts then Nothing else Just Magenta
      printAnnotatedLine (optsFormat opts) color line output
      annotateIO opts input output
