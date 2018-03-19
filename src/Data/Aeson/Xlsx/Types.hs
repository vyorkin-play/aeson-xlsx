{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.Xlsx.Types where

import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Lens (makeLenses)

data BorderStyle
  = BorderStyleNone
  | BorderStyleSolid
  | BorderStyleDouble
  | BorderStyleDashed
  | BorderStyleDotted
  deriving (Eq)

instance Show BorderStyle where
  show BorderStyleNone   = "none"
  show BorderStyleSolid  = "solid"
  show BorderStyleDouble = "double"
  show BorderStyleDashed = "dashed"
  show BorderStyleDotted = "dotted"

data Border = Border
  { _borderColor ∷ Maybe Text
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

data Alignment
  = AlignmentCenter
  | AlignmentSpaceBetween
  | AlignmentStretch
  | AlignmentStart
  | AlignmentEnd
  deriving (Eq)

instance Show Alignment where
  show AlignmentCenter       = "center"
  show AlignmentSpaceBetween = "space-between"
  show AlignmentStretch      = "stretch"
  show AlignmentStart        = "start"
  show AlignmentEnd          = "end"

data CellAlignment = CellAlignment
  { _cellAlignmentHorizontal ∷ Maybe Alignment
  , _cellAlignmentVertical   ∷ Maybe Alignment
  } deriving (Eq, Show, Generic)

makeLenses ''CellAlignment

data CellFont = CellFont
  { _cellFontFamily ∷ Maybe String
  , _cellFontBold   ∷ Maybe Bool
  , _cellFontItalic ∷ Maybe Bool
  } deriving (Eq, Show, Generic)

makeLenses ''CellFont

data CellStyle = CellStyle
  { _cellStyleBorder     ∷ Maybe CellBorder
  , _cellStyleAlignment  ∷ Maybe CellAlignment
  , _cellStyleFont       ∷ Maybe CellFont
  , _cellStyleColor      ∷ Maybe Text
  , _cellStyleBackground ∷ Maybe Text
  } deriving (Eq, Show, Generic)

makeLenses ''CellStyle

data Cell = Cell
  { _cellRow     ∷ Int
  , _cellCol     ∷ Int
  , _cellValue   ∷ Maybe Text
  , _cellFormula ∷ Maybe Text
  , _cellStyle   ∷ Maybe CellStyle
  } deriving (Eq, Show, Generic)

makeLenses ''Cell
