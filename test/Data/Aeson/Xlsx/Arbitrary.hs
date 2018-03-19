module Data.Aeson.Xlsx.Arbitrary where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import Data.Aeson.Xlsx.Types

instance Arbitrary BorderStyle where
  arbitrary = elements
    [ BorderStyleNone
    , BorderStyleSolid
    , BorderStyleDouble
    , BorderStyleDashed
    , BorderStyleDotted
    ]

instance Arbitrary BorderSide where
  arbitrary = elements
    [ BorderLeft
    , BorderRight
    , BorderTop
    , BorderBottom
    ]

instance Arbitrary Border where
  arbitrary = Border
    <$> arbitrary
    <*> elements colors
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

instance Arbitrary CellFont where
  arbitrary = CellFont
    <$> elements fontFamilies
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
    <*> elements colors
    <*> elements colors

instance Arbitrary Cell where
  arbitrary = do
    Positive row ← arbitrary
    Positive col ← arbitrary
    style ← arbitrary
    value ← elements values
    formula ← elements formulas
    return $ Cell row col style value formula
    where
      values =
        [ Nothing
        , Just "foo"
        , Just "bar"
        , Just "baz"
        ]
      formulas =
        [ Nothing
        , Just "SUBNM($C10, $D1)"
        , Just "TM1RPTROW(\"Whatever\", $C12)"
        , Just "DBRW($C5, $B2)"
        ]

colors ∷ [Maybe Text]
colors = [ Nothing
         , Just "#000000"
         , Just "#111111"
         , Just "#222222"
         ]
