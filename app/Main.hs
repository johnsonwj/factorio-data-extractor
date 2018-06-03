module Main where

import FactorioLua

import Foreign.Lua ( runLuaEither )

main :: IO ()
main = do
  result <- runLuaEither loadFactorioData
  case result of
    Left exc  -> putStrLn $ show exc
    Right _   -> putStrLn "success!"
