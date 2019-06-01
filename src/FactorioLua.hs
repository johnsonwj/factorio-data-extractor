
module FactorioLua (loadFactorioData) where

import Foreign.Lua
  ( Lua
  , openlibs
  , getglobal
  , getfield
  , setfield
  , pop
  , peek
  , push
  , liftIO
  , stackTop
  , callFunc
  , raiseError
  , addfunction
  , peekKeyValuePairs
  , ltype
  , Peekable
  , Type(..)
  , isinteger
  )

import System.FilePath ((</>))

import Data.ByteString (ByteString, intercalate)
import Data.ByteString.Char8 (pack, unpack)
import Data.Aeson (ToJSON, toJSON, ToJSONKey)
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Aeson as J
import qualified Data.Map as Map
import qualified Data.Text as T

factorioDataPathRoot :: ByteString
factorioDataPathRoot = "/mnt/e/SteamLibrary/steamapps/common/Factorio/data"

factorioDataModulePattern :: ByteString -> ByteString
factorioDataModulePattern base = pack $ unpack base </> "?.lua"

factorioDataSubmodulePath :: ByteString -> ByteString
factorioDataSubmodulePath moduleName = pack $ unpack factorioDataPathRoot </> unpack moduleName

allFactorioDataModulePaths :: [ByteString]
allFactorioDataModulePaths = map factorioDataModulePattern (factorioDataPathRoot : map factorioDataSubmodulePath pathModules)
    where
      pathModules = ["base", "core", pack $ "core" </> "lualib"]

addFactorioDataPath :: Lua ()
addFactorioDataPath = do
  getglobal "package"
  getfield stackTop "path"
  currentPath <- peek stackTop
  pop 1
  push $ intercalate ";" (currentPath : "./lua/?.lua" : allFactorioDataModulePaths)
  setfield (-2) "path"
  pop 1

luaPrintDbg :: String -> Lua ()
luaPrintDbg = liftIO . putStrLn

require :: String -> Lua ()
require moduleName = do
  success <- callFunc "require" moduleName :: Lua Bool
  if success
    then do
      luaPrintDbg $ "successfully loaded " ++ moduleName
      return ()
    else do
      luaPrintDbg $ "failed to load " ++ moduleName
      raiseError ("failed to load module" :: ByteString)
      return ()

polyfill :: Lua ()
polyfill = do
  getglobal "math"
  addfunction "pow" mathPowPolyfill
  pop 1

  where
    mathPowPolyfill x y = return (x ** y) :: Lua Float

data LuaValue =
  LVNil
  | LVBool Bool
  | LVInt Int
  | LVFloat Float
  | LVString ByteString
  | LVTable [(LuaValue, LuaValue)]
  deriving (Show, Eq, Ord)

instance Peekable LuaValue where
  peek idx = do
    lvType <- ltype idx
    case lvType of
      TypeBoolean -> LVBool <$> (peek idx :: Lua Bool)
      TypeNumber  -> intOrFloat
      TypeString  -> LVString <$> (peek idx :: Lua ByteString)
      TypeTable   -> LVTable <$> peekKeyValuePairs idx
      _           -> return LVNil

    where
      intOrFloat = do
        isInt <- isinteger idx
        if isInt
          then LVInt <$> (peek idx :: Lua Int)
          else LVFloat <$> (peek idx :: Lua Float)

instance ToJSONKey LuaValue where
  toJSONKey = toJSONKeyText lvToText
    where
      showText :: (Show a) => a -> T.Text
      showText = T.pack . show

      lvToText (LVBool v) = showText v
      lvToText (LVInt v) = showText v
      lvToText (LVFloat v) = showText v
      lvToText (LVString v) = T.pack (unpack v)
      lvToText (LVTable v) = error "poop"

instance ToJSON LuaValue where
  toJSON LVNil = J.Null
  toJSON (LVBool v) = toJSON v
  toJSON (LVInt v) = toJSON v
  toJSON (LVFloat v) = toJSON v
  toJSON (LVString v) = toJSON $ unpack v
  toJSON (LVTable kv)
    | hasOnlyIntKeys  = toJSON $ snd <$> kv
    | otherwise       = toJSON $ Map.fromList kv
    where
      hasIntKey (LVInt _, _)  = True
      hasIntKey _             = False

      hasOnlyIntKeys = and (hasIntKey <$> kv)

loadFactorioData :: Lua J.Value
loadFactorioData = do
  openlibs
  polyfill
  addFactorioDataPath
  require "defines"
  require "core.lualib.dataloader"
  require "core.lualib.util"
  require "core.data"
  require "base.data"
  require "base.data-updates"
  getglobal "data"
  getfield stackTop "raw"
  dataValue <- peek stackTop :: Lua LuaValue
  return (toJSON dataValue)
