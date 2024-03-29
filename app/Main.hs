import Annodate (Color, Options (Options), annotateIO)
import Data.Char (toLower, toUpper)
import GHC.IO.Handle
  ( BufferMode (NoBuffering),
    hSetBinaryMode,
    hSetBuffering,
  )
import Options.Applicative
  ( Parser,
    auto,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    option,
    optional,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<**>),
  )
import System.IO (stdin, stdout)
import Text.Read (readMaybe)

parseColor :: String -> Either String Color
parseColor color = maybe (Left $ "Invalid color: " <> color) Right $ readMaybe (capitalize color)
  where
    capitalize (x : xs) = toUpper x : map toLower xs
    capitalize [] = []

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( option
          (eitherReader parseColor)
          ( long "color"
              <> short 'c'
              <> help "Format using color"
          )
      )
    <*> switch (long "no-color" <> short 'C' <> help "Do not use colors")
    <*> strOption
      ( long "format"
          <> short 'f'
          <> value "%F %T"
          <> help "Specify timestamp format (strftime)"
      )
    <*> ( option
            auto
            ( long "inactivity-timeout"
                <> short 'i'
                <> value 5
                <> help "Threshold for pause in the input in seconds"
            )
        )
    <*> switch (long "no-pause" <> short 'P' <> help "Disable showing pauses in the input")

main :: IO ()
main = do
  opts <-
    execParser $
      (optionsParser <**> helper)
        `info` (fullDesc <> progDesc "Annodate - Prepend timestamps to stdio")
  hSetBinaryMode stdin False
  hSetBuffering stdin NoBuffering
  annotateIO opts stdin stdout
