module Main where

import Prelude.Unicode
import Debug.Trace (trace)
import System.Environment (getArgs)
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad ((>=>), guard, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^?), (^.), view)
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import Data.Default (def)
import Data.Typeable
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Types.StyleSheet as X
import qualified Codec.Xlsx.Formatted as X
import Text.Groom
import Data.Aeson.Xlsx

main ∷ IO ()
main = do
  args ← getArgs
  guard $ not ∘ null $ args
  (xlsx, stylesheet) ← load $ head args
  let worksheet   = fromJust $ xlsx ^? X.ixSheet sheetName
      cells       = worksheet ^. X.wsCells
      merges      = worksheet ^. X.wsMerges
      full        = X.toFormattedCells cells merges stylesheet
      significant = significantCells full
  -- putStrLn "full:"
  -- putStrLn $ groom $ full
  -- putStrLn "significant:"
  -- putStrLn $ groom $ significant
  -- putStrLn $ groom $ Map.lookup (2, 2) significant
  LBSC8.putStrLn $ encode $ toMatrix significant
  return ()
    where sheetName = "Лист1"
