{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Aeson.Xlsx.Types where

import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Lens (makeLenses)

data BorderStyle
  = BorderStyleNone
  | BorderStyleSolid
  | BorderStyleDouble
  | BorderStyleDashed
  | BorderStyleDotted
  deriving (Eq, Show, Generic)

data BorderSide
  = BorderLeft
  | BorderRight
  | BorderTop
  | BorderBottom
  deriving (Eq, Show, Generic)

data Border = Border
  { _borderSide  ∷ BorderSide
  , _borderColor ∷ Maybe Text
  , _borderStyle ∷ Maybe BorderStyle
  } deriving (Eq, Show, Generic)

makeLenses ''Border

data CellBorder = CellBorder
  { _borderLeft   ∷ Maybe Border
  , _borderRight  ∷ Maybe Border
  , _borderTop    ∷ Maybe Border
  , _borderBottom ∷ Maybe Border
  } deriving (Eq, Show, Generic)

makeLenses ''CellBorder

pattern CellBorderNil =
  CellBorder Nothing Nothing Nothing Nothing

cellBorderNil ∷ CellBorder → Bool
cellBorderNil CellBorderNil = True
cellBorderNil _             = False

data Alignment
  = AlignmentCenter
  | AlignmentSpaceBetween
  | AlignmentStretch
  | AlignmentStart
  | AlignmentEnd
  deriving (Eq, Show, Generic)

data CellAlignment = CellAlignment
  { _cellAlignmentHorizontal ∷ Maybe Alignment
  , _cellAlignmentVertical   ∷ Maybe Alignment
  } deriving (Eq, Show, Generic)

makeLenses ''CellAlignment

pattern CellAlignmentNil =
  CellAlignment Nothing Nothing

cellAlignmentNil ∷ CellAlignment → Bool
cellAlignmentNil CellAlignmentNil = True
cellAlignmentNil _                = False

data CellFont = CellFont
  { _cellFontFamily ∷ Maybe String
  , _cellFontBold   ∷ Maybe Bool
  , _cellFontItalic ∷ Maybe Bool
  } deriving (Eq, Show, Generic)

makeLenses ''CellFont

pattern CellFontNil =
  CellFont Nothing Nothing Nothing

cellFontNil ∷ CellFont → Bool
cellFontNil CellFontNil = True
cellFontNil _       = False

data CellStyle = CellStyle
  { _cellStyleBorder     ∷ Maybe CellBorder
  , _cellStyleAlignment  ∷ Maybe CellAlignment
  , _cellStyleFont       ∷ Maybe CellFont
  , _cellStyleColor      ∷ Maybe Text
  , _cellStyleBackground ∷ Maybe Text
  } deriving (Eq, Show, Generic)

makeLenses ''CellStyle

pattern CellStyleNil =
  CellStyle Nothing Nothing Nothing Nothing Nothing

cellStyleNil ∷ CellStyle → Bool
cellStyleNil CellStyleNil = True
cellStyleNil _            = False

data Cell = Cell
  { _cellRow     ∷ Int
  , _cellCol     ∷ Int
  , _cellStyle   ∷ Maybe CellStyle
  , _cellValue   ∷ Maybe Text
  , _cellFormula ∷ Maybe Text
  } deriving (Eq, Show, Generic)

makeLenses ''Cell
