module Main where

import           FactorioLua

import           Foreign.Lua                    ( runEither )
import           FactorioData
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Prelude                 hiding ( writeFile )
import           Data.Text                     as T
import           Data.HashMap.Strict           as M

main :: IO ()
main = do
    result <- runEither loadFactorioData
    case result of
        Left  exc -> print exc
        Right v   -> doStuff v

doStuff :: Value -> IO ()
doStuff v = case (fromJSON v :: Result FactorioData) of
    Error   e -> putStrLn $ "error: " ++ e
    Success d -> do
        putStrLn "success!"
        factorioVersion <- getFactorioVersion
        let dataJsonValue = toJSON d
        writeFile "data.json"
            $ encodePretty (addVersion factorioVersion dataJsonValue)

addVersion :: Text -> Value -> Value
addVersion v (Object obj) = Object $ insert "version" (String v) obj
addVersion _ val          = val

printStuff :: Value -> IO ()
printStuff v = case v of
    String s -> putStrLn $ "string: length " ++ show (T.length s)
    Object o -> putStrLn $ "obj: size " ++ show (M.size o)
    _        -> putStrLn "some other garbage"
