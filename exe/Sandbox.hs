module Main where

import Prelude.Unicode
import Debug.Trace (trace)
import System.Environment (getArgs)
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad ((>=>), guard, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^?), (^.), view)
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import Data.Default (def)
import Data.Typeable
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Codec.Xlsx
import Codec.Xlsx.Types.StyleSheet
import Codec.Xlsx.Formatted (FormattedCell, toFormattedCells, formattedCell)
import Text.Groom
import Data.Aeson.Xlsx

data AesonXlsxError
  = ConversionError
  | UnknownError
  deriving (Show, Typeable)

instance Exception AesonXlsxError

type FormattedCellMap = Map (Int, Int) FormattedCell

load ∷ FilePath → IO (Xlsx, StyleSheet)
load file = do
  xlsx ← liftIO $ toXlsx <$> LBS.readFile file
  let styles = xlsx ^. xlStyles
  stylesheet ← either throwIO return $ parseStyleSheet styles
  return (xlsx, stylesheet)

main ∷ IO ()
main = do
  args ← getArgs
  guard $ not ∘ null $ args
  (xlsx, stylesheet) ← load $ head args
  let worksheet   = fromJust $ xlsx ^? ixSheet sheetName
      cells       = worksheet ^. wsCells
      merges      = worksheet ^. wsMerges
      full        = toFormattedCells cells merges stylesheet
      significant = significantCells full
  -- putStrLn "full:"
  -- putStrLn $ groom $ full
  -- putStrLn "significant:"
  -- putStrLn $ groom $ significant
  -- putStrLn $ groom $ Map.lookup (2, 2) significant
  LBSC8.putStrLn $ encode $ toMatrix significant
  return ()
    where sheetName = "Лист1"

-- style
-- formula
-- value

type Coords = (Int, Int)
type Dimensions = (Coords, Coords)

dimensions ∷ FormattedCellMap → Dimensions
dimensions cells = ((x1, y1), (x2, y2))
  where
    x1   = minimum cols
    y1   = minimum rows
    x2   = maximum cols
    y2   = maximum rows
    rows = fst <$> keys
    cols = snd <$> keys
    keys = Map.keys cells

rowCells ∷ FormattedCellMap → [Int] → Int → [FormattedCell]
rowCells cells cols row = fromMaybe def ∘ cellAt <$> cols
  where
    cellAt col = cells !? (row, col)

-- [i, j]

toMatrix ∷ FormattedCellMap → [[FormattedCell]]
toMatrix cells = rowCells cells cols <$> rows
  where
    rows = fst <$> keys
    cols = snd <$> keys
    keys = Map.keys cells

significantCells ∷ FormattedCellMap → FormattedCellMap
significantCells = Map.filter hasFilterOrFormula
  where
    hasFilterOrFormula = (||) <$> hasValue <*> hasFormula
    hasValue           = isJust ∘ (view $ formattedCell ∘ cellValue)
    hasFormula         = isJust ∘ (view $ formattedCell ∘ cellFormula)

type Result' = Either String Int

ex1 ∷ Maybe Int
ex1 = do
  x ← Just 5
  y ← Nothing
  z ← Just $ x + y
  return x

printResult ∷ Result' → IO ()
printResult r = do
  case r of
    Left e  → putStrLn $ "error: " ++ e
    Right v → putStrLn $ "value: " ++ (show v)

ex2 ∷ Result'
ex2 = do
  z ← Right 5
  w ← Left "fuck"
  return $ z + w

ex3 ∷ ExceptT String IO Int
ex3 = ExceptT ∘ return $ ex2

ex3' ∷ IO ()
ex3' = do
  result ← runExceptT ex3
  printResult result
