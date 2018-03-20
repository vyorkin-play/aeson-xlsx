module Data.Aeson.Xlsx.JSON where

import Prelude.Unicode
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Foldable (asum)
import qualified Data.Text as T
import Control.Monad (mzero)
import Control.Lens (Lens', (^?), (^.), to, views)
import Control.Lens.Prism (_Just)
import Data.Aeson (Value(..), ToJSON, FromJSON, toJSON, parseJSON, object, withText, withObject, (.=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Xlsx.Types
import Data.Aeson.Xlsx.Utils (omitNulls)

instance ToJSON Color where
  toJSON (Color x) = toJSON x

instance FromJSON Color where
  parseJSON = withText "Color" $ pure ∘ Color

instance ToJSON CellValue where
  toJSON (StringValue x) = String x
  toJSON (NumberValue x) = Number x
  toJSON (BoolValue x)   = Bool x

instance FromJSON CellValue where
  parseJSON (String x) = pure $ StringValue x
  parseJSON (Number x) = pure $ NumberValue x
  parseJSON (Bool x)   = pure $ BoolValue x
  parseJSON _          = fail "Expected either String, Number or Bool"

instance ToJSON CellFormula where
  toJSON (CellFormula x) = toJSON x

instance FromJSON CellFormula where
  parseJSON = withText "CellFormula" $ pure ∘ CellFormula

-- | `BorderStyle` to CSS `border-style` property.
instance ToJSON BorderStyle where
  toJSON BorderStyleNone   = "none"
  toJSON BorderStyleSolid  = "solid"
  toJSON BorderStyleDouble = "double"
  toJSON BorderStyleDashed = "dashed"
  toJSON BorderStyleDotted = "dotted"

-- | `BorderStyle` from CSS `border-style` property.
instance FromJSON BorderStyle where
  parseJSON = withText expected parse
    where
      parse "none"   = pure BorderStyleNone
      parse "solid"  = pure BorderStyleSolid
      parse "double" = pure BorderStyleDouble
      parse "dashed" = pure BorderStyleDashed
      parse "dotted" = pure BorderStyleDotted
      parse invalid  = typeMismatch expected $ String invalid
      expected       = "BorderStyle"

instance ToJSON Border where
  toJSON Border {..} = omitNulls
    [ "color" .= _borderColor
    , "style" .= _borderStyle
    ]

instance FromJSON Border where
  parseJSON = withObject "Border" $ \o →
    Border <$> o .:? "color"
           <*> o .:? "style"

instance ToJSON CellBorder where
  toJSON CellBorder {..} = omitNulls
    [ "left"   .= _borderLeft
    , "right"  .= _borderRight
    , "top"    .= _borderTop
    , "bottom" .= _borderBottom
    ]

instance FromJSON CellBorder where
  parseJSON = withObject "CellBorder" $ \o →
    CellBorder <$> o .:? "left"
               <*> o .:? "right"
               <*> o .:? "top"
               <*> o .:? "bottom"

instance ToJSON Alignment where
  toJSON AlignmentCenter       = "center"
  toJSON AlignmentSpaceBetween = "space-between"
  toJSON AlignmentStretch      = "stretch"
  toJSON AlignmentStart        = "start"
  toJSON AlignmentEnd          = "end"

instance FromJSON Alignment where
  parseJSON = withText expected parse
    where
      parse "center"        = pure AlignmentCenter
      parse "space-between" = pure AlignmentSpaceBetween
      parse "stretch"       = pure AlignmentStretch
      parse "start"         = pure AlignmentStart
      parse "end"           = pure AlignmentEnd
      parse invalid         = typeMismatch expected $ String invalid
      expected              = "Alignment"

instance ToJSON UnderlineStyle where
  toJSON UnderlineStyleSolid  = "solid"
  toJSON UnderlineStyleDouble = "double"
  toJSON UnderlineStyleDotted = "dotted"
  toJSON UnderlineStyleDashed = "dashed"
  toJSON UnderlineStyleWavy   = "wavy"

instance FromJSON UnderlineStyle where
  parseJSON = withText expected parse
    where
      parse "solid"  = pure UnderlineStyleSolid
      parse "double" = pure UnderlineStyleDouble
      parse "dotted" = pure UnderlineStyleDotted
      parse "dashed" = pure UnderlineStyleDashed
      parse "wavy"   = pure UnderlineStyleWavy
      parse invalid  = typeMismatch expected $ String invalid
      expected       = "UnderlineStyle"

instance ToJSON CellAlignment where
  toJSON CellAlignment {..} = omitNulls
    [ "horizontal" .= _cellAlignmentHorizontal
    , "vertical"   .= _cellAlignmentVertical
    ]

instance FromJSON CellAlignment where
  parseJSON = withObject "CellAlignment" $ \o →
    CellAlignment <$> o .:? "horizontal"
                  <*> o .:? "vertical"

instance ToJSON CellFont where
  toJSON CellFont {..} = omitNulls
    [ "family"    .= _cellFontFamily
    , "bold"      .= _cellFontBold
    , "italic"    .= _cellFontItalic
    , "underline" .= _cellFontUnderline
    ]

instance FromJSON CellFont where
  parseJSON = withObject "CellFont" $ \o →
    CellFont <$> o .:? "family"
             <*> o .:? "bold"
             <*> o .:? "italic"
             <*> o .:? "underline"

instance ToJSON CellStyle where
  toJSON CellStyle {..} = omitNulls
    [ "border"     .= _cellStyleBorder
    , "alignment"  .= _cellStyleAlignment
    , "font"       .= _cellStyleFont
    , "word-wrap"  .= _cellStyleWordWrap
    , "color"      .= _cellStyleColor
    , "background" .= _cellStyleBackground
    ]

instance FromJSON CellStyle where
  parseJSON = withObject "CellStyle" $ \o →
    CellStyle <$> o .:? "border"
              <*> o .:? "alignment"
              <*> o .:? "font"
              <*> o .:? "word-wrap"
              <*> o .:? "color"
              <*> o .:? "background"

instance ToJSON Cell where
  toJSON Cell {..} = omitNulls
    [ "row"     .= _cellRow
    , "col"     .= _cellCol
    , "rowSpan" .= _cellRowSpan
    , "colSpan" .= _cellColSpan
    , "style"   .= _cellStyle
    , "value"   .= _cellValue
    , "formula" .= _cellFormula
    ]

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \o →
    Cell <$> o .:  "row"
         <*> o .:  "col"
         <*> o .:? "rowSpan"
         <*> o .:? "colSpan"
         <*> o .:? "style"
         <*> o .:? "value"
         <*> o .:? "formula"
