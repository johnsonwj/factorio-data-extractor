module Main where

import FactorioLua

import Foreign.Lua (runEither)
import FactorioData
import qualified Data.Aeson as J
import Data.Aeson.Types (fromJSON, Result(..), Value(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)
import Data.Text as T
import Data.HashMap.Strict as M

main :: IO ()
main = do
  result <- runEither loadFactorioData
  case result of
    Left exc  -> print exc
    Right v   -> doStuff v

doStuff :: Value -> IO ()
doStuff v = case (fromJSON v :: Result FactorioData) of
  Error e -> putStrLn $ "error: " ++ e
  Success d -> putStrLn "success!" -- writeFile "data.json" $ encodePretty v

printStuff :: Value -> IO ()
printStuff v = case v of
  String s -> putStrLn $ "string: length " ++ show (T.length s)
  Object o -> putStrLn $ "obj: size " ++ show (M.size o)
  _ -> putStrLn "some other garbage"
