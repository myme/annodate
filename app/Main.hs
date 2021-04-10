import Annodate
import Data.Char (toUpper, toLower)
import GHC.IO.Handle
import Options.Applicative
import System.IO (stdin, stdout)
import Text.Read (readMaybe)

data Options = Options
  { optsColor :: Maybe Color
  , optsFormat :: String
  }

parseColor :: String -> Either String Color
parseColor color = maybe (Left $ "Invalid color: " <> color) Right $ readMaybe (capitalize color)
  where capitalize (x:xs) = toUpper x : map toLower xs
        capitalize [] = []

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (option (eitherReader parseColor)
                (  long "color"
                <> short 'c'
                <> value Magenta
                <> help "Use colors"
                ))
  <*> strOption (  long "format"
                <> short 'f'
                <> value "%F %T"
                <> help "Specify timestamp format (strftime)"
                )

main :: IO ()
main = do
  opts <- execParser $ (optionsParser <**> helper) `info`
    (fullDesc <> progDesc "Annodate - Prepend timestamps to stdio")
  hSetBinaryMode stdin False
  hSetBuffering stdin NoBuffering
  annotateIO (optsFormat opts) (optsColor opts) stdin stdout
