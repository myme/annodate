module Annodate
  ( Color(..)
  , Options(..)
  , annotateIO
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, killThread, forkIO, newMVar, modifyMVar_, withMVar)
import Control.Exception (tryJust, bracket)
import Control.Monad (guard, forever, when)
import Data.Text (Text, pack)
import Data.Text.IO (hGetLine, hPutStr, hPutStrLn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.IO.Handle (Handle, hIsTerminalDevice)
import Prelude hiding (getLine, concat, putStrLn)
import System.Console.ANSI
import System.IO.Error (isEOFError)

data Options = Options
  { optsColor :: Maybe Color
  , optsNoColor :: Bool
  , optsFormat :: String
  , optsNoCountPause :: Bool
  }

type DateFormat = String

setColor :: Handle -> Maybe Color -> IO ()
setColor handle = \case
  Nothing -> hSetSGR handle [Reset]
  Just c  -> hSetSGR handle [SetColor Foreground Dull c]

printAnnotatedLine :: DateFormat -> Maybe Color -> Text -> Handle -> IO ()
printAnnotatedLine format color line output = do
  time <- getZonedTime
  setColor output color
  hPutStr output $ pack $ formatTime defaultTimeLocale format time
  setColor output Nothing
  hPutStrLn output $ ": " <> line

withInactivity :: IO a -> (Int -> IO ()) -> (Int -> IO ()) -> IO a
withInactivity action onTick onDone = do
  inactivity <- newMVar 0

  let inactiveTimer = forkIO $ forever $ do
        threadDelay 1000000
        modifyMVar_ inactivity $ \i -> do
          let newVal = i + 1
          onTick newVal
          pure newVal

  bracket inactiveTimer killThread $ \_ -> do
    result <- action
    withMVar inactivity $ \i -> when (i > 0) (onDone i)
    pure result

inactivityMessage :: Int -> Text
inactivityMessage duration = "Inactive for " <> pack (formatDuration duration)
  where formatDuration i | i == 1 = show i <> " second"
                         | otherwise = show i <> " seconds"

annotateIO :: Options -> Handle -> Handle -> IO ()
annotateIO opts input output = do
  let getLine = tryJust (guard . isEOFError) (hGetLine input)

  content <- if optsNoCountPause opts
    then getLine
    else do
      isTTY <- hIsTerminalDevice output
      let onTTYTick duration = do
            hPutStr output "\r"
            setColor output (Just White)
            hPutStr output (inactivityMessage duration)
      let (onInactive, onDone) = if not isTTY
            then (const $ pure (), hPutStrLn output . inactivityMessage)
            else (onTTYTick, const $ hPutStrLn output "")
      withInactivity getLine onInactive onDone

  case content of
    Left  _    -> return ()
    Right line -> do
      let color = optsColor opts <|> if optsNoColor opts then Nothing else Just Magenta
      printAnnotatedLine (optsFormat opts) color line output
      annotateIO opts input output
