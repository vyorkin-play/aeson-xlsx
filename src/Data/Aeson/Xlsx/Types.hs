{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.Xlsx.Types where

import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Lens (makeLenses)

-- | Wrapper type for HEX RGB color.
newtype Color = Color Text
  deriving (Eq, Show, Generic)

data CellValue
  = StringValue Text
  | NumberValue Scientific
  | BoolValue Bool
  deriving (Eq, Show, Generic)

-- | Wrapper type for cell formula.
newtype CellFormula = CellFormula Text
  deriving (Eq, Show, Generic)

data BorderStyle
  = BorderStyleNone
  | BorderStyleSolid
  | BorderStyleDouble
  | BorderStyleDashed
  | BorderStyleDotted
  deriving (Eq, Show, Generic)

data Border = Border
  { _borderColor ∷ Maybe Color
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
  deriving (Eq, Show, Generic)

data CellAlignment = CellAlignment
  { _cellAlignmentHorizontal ∷ Maybe Alignment
  , _cellAlignmentVertical   ∷ Maybe Alignment
  } deriving (Eq, Show, Generic)

makeLenses ''CellAlignment

data UnderlineStyle
  = UnderlineStyleSolid
  | UnderlineStyleDouble
  | UnderlineStyleDotted
  | UnderlineStyleDashed
  | UnderlineStyleWavy
  deriving (Eq, Show, Generic)

data CellFont = CellFont
  { _cellFontFamily    ∷ Maybe Text
  , _cellFontBold      ∷ Maybe Bool
  , _cellFontItalic    ∷ Maybe Bool
  , _cellFontUnderline ∷ Maybe UnderlineStyle
  } deriving (Eq, Show, Generic)

makeLenses ''CellFont

data CellStyle = CellStyle
  { _cellStyleBorder     ∷ Maybe CellBorder
  , _cellStyleAlignment  ∷ Maybe CellAlignment
  , _cellStyleFont       ∷ Maybe CellFont
  , _cellStyleWordWrap   ∷ Maybe Bool
  , _cellStyleColor      ∷ Maybe Color
  , _cellStyleBackground ∷ Maybe Color
  } deriving (Eq, Show, Generic)

makeLenses ''CellStyle

data Cell = Cell
  { _cellRow     ∷ Int
  , _cellCol     ∷ Int
  , _cellRowSpan ∷ Maybe Int
  , _cellColSpan ∷ Maybe Int
  , _cellStyle   ∷ Maybe CellStyle
  , _cellValue   ∷ Maybe CellValue
  , _cellFormula ∷ Maybe CellFormula
  } deriving (Eq, Show, Generic)

makeLenses ''Cell
