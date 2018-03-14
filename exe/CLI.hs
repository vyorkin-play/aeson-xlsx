module Main where

import Options.Applicative
import Data.Semigroup ((<>))

-- | CLI application config.
data Config = Config
  { filePath ∷ String
  , sheetName ∷ Maybe String
  , sheetIndex ∷ Maybe Int
  } deriving (Show)

-- | CLI application config parser.
parser ∷ Parser Config
parser = Config
  <$> strOption
      ( long "filePath"
     <> short 'f'
     <> metavar "FILE_PATH"
     <> help "Path to the XLSX file"
      )
  <*> option auto
     ( long "sheetName"
    <> short 'n'
    <> metavar "SHEET_NAME"
    <> help "Sheet name"
     )
  <*> option auto
     ( long "sheetIndex"
    <> short 'i'
    <> metavar "SHEET_INDEX"
    <> help "Sheet index"
     )


-- | Describes what the program does.
-- | To be displayed in the help screen.
usage ∷ ParserInfo Config
usage = info (parser <**> helper)
  ( fullDesc
 <> progDesc "xlsx2json"
 <> header "xlsx2json - opinionated XLSX to JSON converter"
  )

main ∷ IO ()
main = do
  cfg ← execParser usage
  putStrLn $ show cfg
  return ()
