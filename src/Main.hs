module Main where

import FactorioLua

import Foreign.Lua (runEither)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

main :: IO ()
main = do
  result <- runEither loadFactorioData
  case result of
    Left exc  -> print exc
    Right v   -> writeFile "data.json" $ encodePretty v
