module Data.Aeson.Xlsx.Arbitrary where

import Prelude.Unicode
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import Test.QuickCheck.Instances.Scientific
import Data.Aeson.Xlsx.Types

instance Arbitrary Color where
  arbitrary = Color <$> arbitrary

instance Arbitrary CellValue where
  arbitrary = oneof
    [ StringValue <$> arbitrary
    , NumberValue <$> arbitrary
    , BoolValue   <$> arbitrary
    ]

instance Arbitrary CellFormula where
  arbitrary = CellFormula <$> arbitrary

instance Arbitrary BorderStyle where
  arbitrary = elements
    [ BorderStyleNone
    , BorderStyleSolid
    , BorderStyleDouble
    , BorderStyleDashed
    , BorderStyleDotted
    ]

instance Arbitrary Border where
  arbitrary = Border
    <$> elements colors
    <*> arbitrary

instance Arbitrary CellBorder where
  arbitrary = CellBorder
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Alignment where
  arbitrary = elements
    [ AlignmentCenter
    , AlignmentSpaceBetween
    , AlignmentStretch
    , AlignmentStart
    , AlignmentEnd
    ]

instance Arbitrary CellAlignment where
  arbitrary = CellAlignment
    <$> arbitrary
    <*> arbitrary

instance Arbitrary UnderlineStyle where
  arbitrary = elements
    [ UnderlineStyleSolid
    , UnderlineStyleDouble
    , UnderlineStyleDotted
    , UnderlineStyleDashed
    , UnderlineStyleWavy
    ]

instance Arbitrary CellFont where
  arbitrary = CellFont
    <$> elements fontFamilies
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    where
      fontFamilies =
        [ Nothing
        , Just "Arial"
        , Just "Tahoma"
        , Just "Verdana"
        ]

instance Arbitrary CellStyle where
  arbitrary = CellStyle
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> elements colors
    <*> elements colors

instance Arbitrary Cell where
  arbitrary = do
    Positive row ← arbitrary
    Positive col ← arbitrary
    rowSpan ← elements spans
    colSpan ← elements spans
    style ← arbitrary
    value ← elements values
    formula ← elements formulas
    return $ Cell row col rowSpan colSpan style value formula
    where
      spans =
        [ Nothing
        , Just 1
        , Just 2
        ]
      values =
        [ Nothing
        , Just $ StringValue "foo"
        , Just $ NumberValue $ fromFloatDigits 42.5
        , Just $ BoolValue True
        ]
      formulas =
        [ Nothing
        , Just $ CellFormula "SUBNM($C10, $D1)"
        , Just $ CellFormula "TM1RPTROW(\"Whatever\", $C12)"
        , Just $ CellFormula "DBRW($C5, $B2)"
        ]

colors ∷ [Maybe Color]
colors = [ Nothing
         , Just $ Color "#000000"
         , Just $ Color "#111111"
         , Just $ Color "#222222"
         ]
