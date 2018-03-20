{-# LANGUAGE LambdaCase #-}
module Data.Aeson.Xlsx.Constructors where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (fromFloatDigits)
import Control.Lens (Lens', (^?), (^.), to, views)
import Control.Lens.Prism (_Just)
import qualified Codec.Xlsx.Writer.Internal as X (toAttrVal)
import qualified Codec.Xlsx.Types.StyleSheet as X
import qualified Codec.Xlsx.Types as X
import qualified Codec.Xlsx.Formatted as X
import Data.Aeson.Xlsx.Types

mkCell ∷ (Int, Int) → X.FormattedCell → Cell
mkCell (row, col) x = Cell row col rowSpan colSpan style value formula
  where
    value   = x ^? (X.formattedCell ∘ to mkCellValue ∘ _Just)
    formula = x ^? (X.formattedCell ∘ X.cellFormula ∘ _Just ∘ to mkCellFormula ∘ _Just)
    rowSpan = mkSpan $ x ^. X.formattedRowSpan
    colSpan = mkSpan $ x ^. X.formattedColSpan
    style   = mkCellStyle $ x ^. X.formattedFormat

    mkSpan 1 = Nothing
    mkSpan n = Just n

mkCellValue ∷ X.Cell → Maybe CellValue
mkCellValue x = convert <$> x ^. X.cellValue
  where
    convert ∷ X.CellValue → CellValue
    convert = \case
        (X.CellText x)   → StringValue x
        (X.CellDouble x) → NumberValue $ fromFloatDigits x
        (X.CellBool x)   → BoolValue x
        (X.CellRich x)   → StringValue $ T.concat $ X._richTextRunText <$> x
        (X.CellError x)  → StringValue $ X.toAttrVal x

mkCellFormula ∷ X.CellFormula → Maybe CellFormula
mkCellFormula x = x ^. to (mkFormula ∘ X._cellfExpression)
  where
    mkFormula ∷ X.FormulaExpression → Maybe CellFormula
    mkFormula (X.NormalFormula f) = Just $ CellFormula $ X.unFormula f
    mkFormula (X.SharedFormula _) = Nothing

mkCellStyle ∷ X.Format → Maybe CellStyle
mkCellStyle x = mkStyle border alignment font wordWrap color bg
  where
    border    = x ^? (X.formatBorder ∘ _Just ∘ to mkCellBorder)
    alignment = x ^? (X.formatAlignment ∘ _Just ∘ to mkCellAlignment ∘ _Just)
    font      = x ^? (X.formatFont ∘ _Just ∘ to mkCellFont ∘ _Just)
    wordWrap  = x ^? (X.formatAlignment ∘ _Just ∘ X.alignmentWrapText ∘ _Just)
    color     = x ^? (X.formatFont ∘ _Just ∘ X.fontColor ∘ _Just ∘ to mkColor)
    bg        = x ^? (X.formatFill ∘ _Just ∘ X.fillPattern ∘ _Just ∘ X.fillPatternBgColor ∘ _Just ∘ to mkColor)

    mkStyle Nothing Nothing Nothing Nothing Nothing Nothing = Nothing
    mkStyle border alignment font wordWrap color bg =
      Just $ CellStyle border alignment font wordWrap color bg

mkBorderStyle ∷ X.LineStyle → Maybe BorderStyle
mkBorderStyle = \case
    X.LineStyleDashDot          → Just BorderStyleDashed
    X.LineStyleDashDotDot       → Just BorderStyleDotted
    X.LineStyleDashed           → Just BorderStyleDashed
    X.LineStyleDotted           → Just BorderStyleDotted
    X.LineStyleDouble           → Just BorderStyleDouble
    X.LineStyleHair             → Just BorderStyleSolid
    X.LineStyleMedium           → Just BorderStyleSolid
    X.LineStyleMediumDashDot    → Just BorderStyleDashed
    X.LineStyleMediumDashDotDot → Just BorderStyleDotted
    X.LineStyleMediumDashed     → Just BorderStyleDashed
    X.LineStyleSlantDashDot     → Just BorderStyleDashed
    X.LineStyleThick            → Just BorderStyleSolid
    X.LineStyleThin             → Just BorderStyleSolid
    _                           → Nothing

mkCellBorder ∷ X.Border → CellBorder
mkCellBorder x = CellBorder l r t b
  where
    l = mkBorder (color X.borderLeft)   (style X.borderLeft)
    r = mkBorder (color X.borderRight)  (style X.borderRight)
    t = mkBorder (color X.borderTop)    (style X.borderTop)
    b = mkBorder (color X.borderBottom) (style X.borderBottom)

    color l = x ^? (l ∘ _Just ∘ X.borderStyleColor ∘ _Just ∘ to mkColor)
    style l = x ^? (l ∘ _Just ∘ X.borderStyleLine ∘ _Just ∘ to mkBorderStyle ∘ _Just)

mkBorder ∷ Maybe Color → Maybe BorderStyle → Maybe Border
mkBorder Nothing Nothing = Nothing
mkBorder color style     = Just $ Border color style

mkUnderlineStyle ∷ X.FontUnderline → Maybe UnderlineStyle
mkUnderlineStyle = \case
  X.FontUnderlineSingle           → Just UnderlineStyleSolid
  X.FontUnderlineDouble           → Just UnderlineStyleDouble
  X.FontUnderlineSingleAccounting → Just UnderlineStyleSolid
  X.FontUnderlineDoubleAccounting → Just UnderlineStyleDouble
  X.FontUnderlineNone             → Nothing

mkCellFont ∷ X.Font → Maybe CellFont
mkCellFont x = mkFont family bold italic underline
  where
    family    = x ^. X.fontName
    bold      = x ^. X.fontBold
    italic    = x ^. X.fontItalic
    underline = x ^? (X.fontUnderline ∘ _Just ∘ to mkUnderlineStyle ∘ _Just)

    mkFont Nothing Nothing Nothing Nothing = Nothing
    mkFont family bold italic underline =
      Just $ CellFont family bold italic underline

mkVerticalAlignment ∷ X.CellVerticalAlignment → Alignment
mkVerticalAlignment = \case
  X.CellVerticalAlignmentBottom      → AlignmentEnd
  X.CellVerticalAlignmentCenter      → AlignmentCenter
  X.CellVerticalAlignmentDistributed → AlignmentSpaceBetween
  X.CellVerticalAlignmentJustify     → AlignmentStretch
  X.CellVerticalAlignmentTop         → AlignmentStart

mkHorizontalAlignment ∷ X.CellHorizontalAlignment → Maybe Alignment
mkHorizontalAlignment = \case
  X.CellHorizontalAlignmentCenter           → Just AlignmentCenter
  X.CellHorizontalAlignmentCenterContinuous → Just AlignmentCenter
  X.CellHorizontalAlignmentDistributed      → Just AlignmentSpaceBetween
  X.CellHorizontalAlignmentFill             → Just AlignmentSpaceBetween
  X.CellHorizontalAlignmentJustify          → Just AlignmentStretch
  X.CellHorizontalAlignmentLeft             → Just AlignmentStart
  X.CellHorizontalAlignmentRight            → Just AlignmentEnd
  _                                         → Nothing

mkCellAlignment ∷ X.Alignment → Maybe CellAlignment
mkCellAlignment x = mkAlignment h v
  where
    v = x ^? (X.alignmentHorizontal ∘ _Just ∘ to mkHorizontalAlignment ∘ _Just)
    h = x ^? (X.alignmentVertical ∘ _Just ∘ to mkVerticalAlignment)

    mkAlignment Nothing Nothing = Nothing
    mkAlignment v h             = Just $ CellAlignment v h

-- | `Color` to RGB string value.
mkColor ∷ X.Color → Color
mkColor x = Color $ views (X.colorARGB ∘ _Just) convert x
  where
    convert = T.cons '#' ∘ T.drop 2
