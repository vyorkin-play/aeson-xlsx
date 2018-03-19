module Data.Aeson.Xlsx where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens (Lens', (^?), (^.), to, views)
import Control.Lens.Prism (_Just)
import Data.Scientific (fromFloatDigits)
import Data.Aeson
import Codec.Xlsx
import Codec.Xlsx.Formatted
import Codec.Xlsx.Writer.Internal (toAttrVal)
import Data.Aeson.Xlsx.Utils (omitNulls)

instance ToJSON FormattedCell where
  toJSON FormattedCell {..} = omitNulls
    [ "cell"    .= _formattedCell
    , "style"   .= _formattedFormat
    , "colSpan" .= toSpan _formattedColSpan
    , "rowSpan" .= toSpan _formattedRowSpan
    ]
    where
      toSpan 1 = Nothing
      toSpan x = Just x

instance ToJSON Cell where
  toJSON x = omitNulls
    [ "value"   .= (x ^. cellValue)
    , "formula" .= (x ^? formula)
    ]
    where
      formula = cellFormula ∘ _Just ∘ to (formulaText ∘ _cellfExpression) ∘ _Just
      formulaText ∷ FormulaExpression → Maybe Text
      formulaText (NormalFormula f) = Just $ unFormula f
      formulaText (SharedFormula _) = Nothing

instance ToJSON CellValue where
  toJSON (CellText x)   = String x
  toJSON (CellDouble x) = Number $ fromFloatDigits x
  toJSON (CellBool x)   = Bool x
  toJSON (CellRich x)   = String $ T.concat $ _richTextRunText <$> x
  toJSON (CellError x)  = String $ toAttrVal x

-- | `Format` to cell CSS properties.
instance ToJSON Format where
  toJSON x = omitNulls
    [ "align-content"         .= (x ^? formatAlignment ∘ _Just ∘ alignmentHorizontal ∘ _Just)
    , "justify-content"       .= (x ^? formatAlignment ∘ _Just ∘ alignmentVertical ∘ _Just)
    , "word-wrap"             .= views (formatAlignment ∘ _Just ∘ alignmentWrapText ∘ _Just) toWordWrap x
    , "border-left-style"     .= (x ^? (formatBorder ∘ _Just ∘ borderLeft  ∘ _Just ∘ borderStyleLine ∘ _Just))
    , "border-right-style"    .= (x ^? (formatBorder ∘ _Just ∘ borderRight ∘ _Just ∘ borderStyleLine ∘ _Just))
    , "border-left-color"     .= (x ^? (formatBorder ∘ _Just ∘ borderLeft  ∘ _Just ∘ borderStyleColor ∘ _Just))
    , "border-right-color"    .= (x ^? (formatBorder ∘ _Just ∘ borderRight ∘ _Just ∘ borderStyleColor ∘ _Just))
    , "font-family"           .= (x ^? (formatFont ∘ _Just ∘ fontName ∘ _Just))
    , "font-weight"           .= views (formatFont ∘ _Just ∘ fontBold ∘ _Just) toFontWeight x
    , "font-style"            .= views (formatFont ∘ _Just ∘ fontItalic ∘ _Just) toFontStyle x
    , "color"                 .= (x ^? (formatFont ∘ _Just ∘ fontColor ∘ _Just))
    , "text-decoration-style" .= (x ^? (formatFont ∘ _Just ∘ fontUnderline ∘ _Just))
    , "vertical-align"        .= (x ^? (formatFont ∘ _Just ∘ fontVertAlign ∘ _Just))
    , "background-color"      .= (x ^? (formatFill ∘ _Just ∘ fillPattern ∘ _Just ∘ fillPatternBgColor ∘ _Just))
    ]
    where
      toWordWrap ∷ Bool → Maybe Text
      toWordWrap True  = Just "normal"
      toWordWrap False = Nothing

      toFontWeight ∷ Bool → Maybe Text
      toFontWeight True  = Just "bold"
      toFontWeight False = Nothing

      toFontStyle ∷ Bool → Maybe Text
      toFontStyle True  = Just "italic"
      toFontStyle False = Nothing

-- | `CellHorizontalAlignment` to CSS 'align-content' property.
instance ToJSON CellHorizontalAlignment where
  toJSON CellHorizontalAlignmentCenter           = "center"        -- center
  toJSON CellHorizontalAlignmentCenterContinuous = "center"        -- centerContinuous
  toJSON CellHorizontalAlignmentDistributed      = "space-between" -- distributed
  toJSON CellHorizontalAlignmentFill             = "space-between" -- fill
  toJSON CellHorizontalAlignmentGeneral          = "normal"        -- general
  toJSON CellHorizontalAlignmentJustify          = "stretch"       -- justify
  toJSON CellHorizontalAlignmentLeft             = "start"         -- left
  toJSON CellHorizontalAlignmentRight            = "end"           -- right

-- | `CellVerticalAlignment` to CSS `justify-content` property.
instance ToJSON CellVerticalAlignment where
  toJSON CellVerticalAlignmentBottom      = "end"           -- bottom
  toJSON CellVerticalAlignmentCenter      = "center"        -- center
  toJSON CellVerticalAlignmentDistributed = "space-between" -- distributed
  toJSON CellVerticalAlignmentJustify     = "stretch"       -- justify
  toJSON CellVerticalAlignmentTop         = "start"         -- top

-- | `FontUnderline` to CSS `text-decoration-style` property.
instance ToJSON FontUnderline where
  toJSON FontUnderlineSingle           = "solid"  -- single
  toJSON FontUnderlineDouble           = "double" -- double
  toJSON FontUnderlineSingleAccounting = "solid"  -- singleAccounting
  toJSON FontUnderlineDoubleAccounting = "double" -- doubleAccounting
  toJSON FontUnderlineNone             = "inherit"

-- | `FontVerticalAlignment` to CSS `vertical-align` property.
instance ToJSON FontVerticalAlignment where
  toJSON FontVerticalAlignmentBaseline    = "baseline" -- baseline
  toJSON FontVerticalAlignmentSubscript   = "sub"      -- subscript
  toJSON FontVerticalAlignmentSuperscript = "super"    -- superscript

-- | `Color` to RGBA string value.
instance ToJSON Color where
  toJSON c = String $ views (colorARGB ∘ _Just) toColor c
    where
      toColor = T.cons '#' ∘ T.drop 2

-- | `LineStyle` to CSS `border-style` property.
instance ToJSON LineStyle where
  toJSON LineStyleDashDot          = "dashed"
  toJSON LineStyleDashDotDot       = "dotted"
  toJSON LineStyleDashed           = "dashed"
  toJSON LineStyleDotted           = "dotted"
  toJSON LineStyleDouble           = "double"
  toJSON LineStyleHair             = "solid"
  toJSON LineStyleMedium           = "solid"
  toJSON LineStyleMediumDashDot    = "dashed"
  toJSON LineStyleMediumDashDotDot = "dotted"
  toJSON LineStyleMediumDashed     = "dashed"
  toJSON LineStyleNone             = "none"
  toJSON LineStyleSlantDashDot     = "dashed"
  toJSON LineStyleThick            = "solid"
  toJSON LineStyleThin             = "solid"
