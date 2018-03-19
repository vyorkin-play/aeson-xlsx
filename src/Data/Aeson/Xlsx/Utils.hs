module Data.Aeson.Xlsx.Utils
  ( omitNulls
  ) where

import Prelude.Unicode
import Data.Text (Text)
import Data.Aeson (Value(..), object)

omitNulls ∷ [(Text, Value)] → Value
omitNulls = object ∘ filter notNull
  where
    notNull (_, Null) = False
    notNull _         = True
