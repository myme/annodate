import Control.Exception (tryJust)
import Control.Monad     (guard)
import Data.Maybe        (mapMaybe)
import Data.Time         (FormatTime, defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Handle
import System.Console.GetOpt
import System.Environment
import System.IO       (stdin)
import System.IO.Error (isEOFError)

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

data OptionFlag = FormatFlag String
                | HelpFlag
                deriving (Eq, Show)

options :: [OptDescr OptionFlag]
options = [ Option "f" ["format"] (ReqArg FormatFlag "FORMAT") "date format string"
          , Option "h" ["help"] (NoArg HelpFlag) "display usage info"
          ]

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
    let (opts, _, _) = getOpt RequireOrder options cliArgs
    if HelpFlag `elem` opts
        then usage
        else do
            hSetBinaryMode stdin False
            hSetBuffering stdin NoBuffering
            annotateIO (formatOption opts) stdin
    where usageHeader = "usage: annodate [OPTIONS...]"
          usage = putStr $ usageInfo usageHeader options
