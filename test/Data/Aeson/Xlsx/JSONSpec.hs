module Data.Aeson.Xlsx.JSONSpec (spec) where

import Test.Hspec
import Test.Aeson.GenericSpecs (Settings(..), GoldenDirectoryOption(..), defaultSettings, goldenSpecs)
import Data.Proxy
import Data.Aeson
import Data.Aeson.Xlsx.Types
import Data.Aeson.Xlsx.JSON
import Data.Aeson.Xlsx.Arbitrary

settings ∷ Settings
settings = defaultSettings
  { goldenDirectoryOption = CustomDirectoryName "json-snapshots"
  , useModuleNameAsSubDirectory = True
  , sampleSize = 10
  }

spec ∷ Spec
spec = do
  describe "JSON" $ do
    goldenSpecs settings (Proxy :: Proxy CellBorder)
    goldenSpecs settings (Proxy :: Proxy CellAlignment)
    goldenSpecs settings (Proxy :: Proxy CellFont)
    goldenSpecs settings (Proxy :: Proxy CellStyle)
    goldenSpecs settings (Proxy :: Proxy Cell)
