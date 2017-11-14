import Control.Exception (tryJust)
import Control.Monad     (guard)
import Data.Maybe        (mapMaybe)
import Data.Time         (FormatTime, defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Handle
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO.Error
import System.Process

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
            let output = annotateLine format time line
            putStrLn output
            annotateIO format handle

spawn :: String -> [String] -> IO Handle
spawn cmd args = do
    (_, out, _, _) <- runInteractiveProcess cmd args Nothing Nothing
    return out

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
    let (opts, args, errs) = getOpt RequireOrder options cliArgs
    if HelpFlag `elem` opts
        then usage
        else if null args || not (null errs)
            then do
                putStrLn usageHeader
                putStr . concat $ errs
                exitFailure
            else do
                out <- spawn (head args) (tail args)
                hSetBinaryMode out False
                hSetBuffering out NoBuffering
                annotateIO (formatOption opts) out
    where usageHeader = "usage: annodate [OPTIONS...] <COMMAND> [ARGS...]"
          usage = putStr $ usageInfo usageHeader options