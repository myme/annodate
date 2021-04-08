{-# LANGUAGE LambdaCase #-}

import Annodate
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Char (toUpper, toLower)
import Data.Maybe (mapMaybe)
import GHC.IO.Handle
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin, stdout)
import Text.Read (readMaybe)

data OptionFlag = ColorFlag (Maybe Color)
                | FormatFlag String
                | HelpFlag
                deriving (Eq, Show)

parseColor :: Maybe String -> OptionFlag
parseColor color = ColorFlag $ readMaybe . capitalize =<< color
  where capitalize (x:xs) = toUpper x : map toLower xs
        capitalize [] = []

options :: [OptDescr OptionFlag]
options = [ Option "f" ["format"] (ReqArg FormatFlag "FORMAT") "date FORMAT string"
          , Option "h" ["help"] (NoArg HelpFlag) "display usage info"
          , Option "c" ["color"] (OptArg parseColor "COLOR") "display timestamp in COLOR"
          ]

colorOption :: [OptionFlag] -> Maybe Color
colorOption = \case
  (ColorFlag c:_) -> c <|> Just Magenta
  (_:xs)          -> colorOption xs
  []              -> Nothing

formatOption :: [OptionFlag] -> String
formatOption opts =
  case mapMaybe toFormat opts of
    (x:_) -> x
    _     -> "%F %T"
  where toFormat (FormatFlag f) = Just f
        toFormat _              = Nothing

main :: IO ()
main = do
  cliArgs <- getArgs
  let (opts, _, invalid, _) = getOpt' RequireOrder options cliArgs
  unless (null invalid) $ do
    putStrLn $ "Invalid options: " ++ show invalid
    exitFailure
  if HelpFlag `elem` opts
    then usage
    else do
      hSetBinaryMode stdin False
      hSetBuffering stdin NoBuffering
      annotateIO (formatOption opts) (colorOption opts) stdin stdout
  where usageHeader = "usage: annodate [OPTIONS...]"
        usage = putStr $ usageInfo usageHeader options
