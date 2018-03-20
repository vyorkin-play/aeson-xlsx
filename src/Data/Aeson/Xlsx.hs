module Data.Aeson.Xlsx
  ( module AX
  , toMatrix
  , load
  , significantCells
  ) where

import Prelude.Unicode
import Debug.Trace (trace)
import Control.Exception (throwIO)
import Data.Default (def)
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import Control.Lens ((^?), (^.), view)
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Types.StyleSheet as X
import qualified Codec.Xlsx.Formatted as X
import Data.Aeson.Xlsx.Types as AX
import Data.Aeson.Xlsx.Constructors as AX
import Data.Aeson.Xlsx.JSON as AX

type FormattedCellMap = Map (Int, Int) X.FormattedCell

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

toMatrix ∷ FormattedCellMap → [[Cell]]
toMatrix cells = mkCells cells cols <$> rows
  where
    rows = fst <$> keys
    cols = snd <$> keys
    keys = Map.keys cells

mkCells ∷ FormattedCellMap → [Int] → Int → [Cell]
mkCells cells cols row = catMaybes $ cellAt <$> cols
  where
    cellAt col =
      let pos = (row, col)
       in mkCell pos <$> cells !? pos

-- | Extracts significant cells.
significantCells ∷ FormattedCellMap → FormattedCellMap
significantCells = Map.filter significant
  where
    significant = (||) <$> has X.cellValue <*> has X.cellFormula
    has l = isJust ∘ (view $ X.formattedCell ∘ l)

-- | Loads `Xlsx` from the given file path.
load ∷ FilePath → IO (X.Xlsx, X.StyleSheet)
load file = do
  xlsx ← liftIO $ X.toXlsx <$> LBS.readFile file
  let styles = xlsx ^. X.xlStyles
  stylesheet ← either throwIO return $ X.parseStyleSheet styles
  return (xlsx, stylesheet)
