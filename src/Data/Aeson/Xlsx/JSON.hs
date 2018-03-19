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

instance ToJSON BorderStyle where
  toJSON BorderStyleNone   = "none"
  toJSON BorderStyleSolid  = "solid"
  toJSON BorderStyleDouble = "double"
  toJSON BorderStyleDashed = "dashed"
  toJSON BorderStyleDotted = "dotted"

instance FromJSON BorderStyle where
  parseJSON = withText expected parse
    where
      parse "none"   = return BorderStyleNone
      parse "solid"  = return BorderStyleSolid
      parse "double" = return BorderStyleDouble
      parse "dashed" = return BorderStyleDashed
      parse "dotted" = return BorderStyleDotted
      parse invalid  = typeMismatch expected (String invalid)
      expected       = "BorderStyle"

instance ToJSON BorderSide where
  toJSON BorderLeft   = "left"
  toJSON BorderRight  = "right"
  toJSON BorderTop    = "top"
  toJSON BorderBottom = "bottom"

instance FromJSON BorderSide where
  parseJSON = withText expected parse
    where
      parse "left"   = return BorderLeft
      parse "right"  = return BorderRight
      parse "top"    = return BorderTop
      parse "bottom" = return BorderBottom
      parse invalid  = typeMismatch expected (String invalid)
      expected       = "BorderSide"

instance ToJSON Border where
  toJSON Border {..} = omitNulls
    [ "side"  .= _borderSide
    , "color" .= _borderColor
    , "style" .= _borderStyle
    ]

instance FromJSON Border where
  parseJSON = withObject "Border" $ \o →
    Border <$> o .:  "side"
           <*> o .:? "color"
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
      parse "center"        = return AlignmentCenter
      parse "space-between" = return AlignmentSpaceBetween
      parse "stretch"       = return AlignmentStretch
      parse "start"         = return AlignmentStart
      parse "end"           = return AlignmentEnd
      parse invalid         = typeMismatch expected (String invalid)
      expected              = "Alignment"

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
    [ "family" .= _cellFontFamily
    , "bold"   .= _cellFontBold
    , "italic" .= _cellFontItalic
    ]

instance FromJSON CellFont where
  parseJSON = withObject "CellFont" $ \o →
    CellFont <$> o .:? "family"
             <*> o .:? "bold"
             <*> o .:? "italic"

instance ToJSON CellStyle where
  toJSON CellStyle {..} = omitNulls
    [ "border"     .= border _cellStyleBorder
    , "alignment"  .= alignment _cellStyleAlignment
    , "font"       .= font _cellStyleFont
    , "color"      .= _cellStyleColor
    , "background" .= _cellStyleBackground
    ]
    where
      border ∷ Maybe CellBorder → Maybe CellBorder
      border Nothing                    = Nothing
      border (Just x) | cellBorderNil x = Nothing
                      | otherwise       = Just x

      alignment ∷ Maybe CellAlignment → Maybe CellAlignment
      alignment Nothing                       = Nothing
      alignment (Just x) | cellAlignmentNil x = Nothing
                         | otherwise          = Just x

      font ∷ Maybe CellFont → Maybe CellFont
      font Nothing                  = Nothing
      font (Just x) | cellFontNil x = Nothing
                    | otherwise     = Just x

instance FromJSON CellStyle where
  parseJSON = withObject "CellStyle" $ \o →
    CellStyle <$> o .:? "border"
              <*> o .:? "alignment"
              <*> o .:? "font"
              <*> o .:? "color"
              <*> o .:? "background"

instance ToJSON Cell where
  toJSON Cell {..} = omitNulls
    [ "row"     .= _cellRow
    , "col"     .= _cellCol
    , "style"   .= style _cellStyle
    , "value"   .= _cellValue
    , "formula" .= _cellFormula
    ]
    where
      style ∷ Maybe CellStyle → Maybe CellStyle
      style Nothing                   = Nothing
      style (Just x) | cellStyleNil x = Nothing
                     | otherwise      = Just x

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \o →
    Cell <$> o .:  "row"
         <*> o .:  "col"
         <*> o .:? "style"
         <*> o .:? "value"
         <*> o .:? "formula"
