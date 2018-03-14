module Main where

import Prelude.Unicode
import System.Environment (getArgs)
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad ((>=>), guard, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^?), (^.))
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Typeable
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Codec.Xlsx
import Codec.Xlsx.Formatted (FormattedCell, toFormattedCells)
import Text.Groom

data AesonXlsxError
  = ConversionError
  | UnknownError
  deriving (Show, Typeable)

instance Exception AesonXlsxError

load ∷ FilePath → IO (Xlsx, StyleSheet)
load file = do
  xlsx ← liftIO $ toXlsx <$> L.readFile file
  let styles = xlsx ^. xlStyles
  stylesheet ← either throwIO return (parseStyleSheet styles)
  return (xlsx, stylesheet)

main ∷ IO ()
main = do
  args ← getArgs
  guard $ not ∘ null $ args
  (xlsx, stylesheet) ← load $ head args
  let worksheet = fromJust $ xlsx ^? ixSheet sheetName
      cells     = worksheet ^. wsCells
      merges    = worksheet ^. wsMerges
      fcells    = toFormattedCells cells merges stylesheet
  putStrLn $ groom $ Map.lookup (2, 2) fcells
  return ()
    where sheetName = "форма 1"

type Result' = Either String Int

ex1 ∷ Maybe Int
ex1 = do
  x ← Just 5
  y ← Nothing
  z ← Just $ x + y
  return x

printResult ∷ Result' → IO ()
printResult r = do
  case r of
    Left e  → putStrLn $ "error: " ++ e
    Right v → putStrLn $ "value: " ++ (show v)

ex2 ∷ Result'
ex2 = do
  z ← Right 5
  w ← Left "fuck"
  return $ z + w

ex3 ∷ ExceptT String IO Int
ex3 = ExceptT ∘ return $ ex2

ex3' ∷ IO ()
ex3' = do
  result ← runExceptT ex3
  printResult result
