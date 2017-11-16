import           Annodate
import           Control.Monad         (unless)
import           Data.Maybe            (mapMaybe)
import           GHC.IO.Handle
import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)
import           System.IO             (stdin)

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
    let (opts, _, invalid, _) = getOpt' RequireOrder options cliArgs
    unless (null invalid) $ do
      putStrLn $ "Invalid options: " ++ (show invalid)
      exitFailure
    if HelpFlag `elem` opts
        then usage
        else do
            hSetBinaryMode stdin False
            hSetBuffering stdin NoBuffering
            annotateIO (formatOption opts) stdin
    where usageHeader = "usage: annodate [OPTIONS...]"
          usage = putStr $ usageInfo usageHeader options
