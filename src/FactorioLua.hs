{-# LANGUAGE OverloadedStrings #-}

module FactorioLua
  ( loadFactorioData
  ) where

import Foreign.Lua
  ( Lua
  , openlibs
  , dostring
  , getglobal
  , getfield
  , setfield
  , pop
  , remove
  , call
  , peek
  , push
  )

import System.FilePath ((</>))

import Data.ByteString (ByteString, intercalate)
import Data.ByteString.Char8 (pack)

factorioDataPath :: ByteString
factorioDataPath = pack "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Factorio\\data"

addFactorioDataPath :: Lua ()
addFactorioDataPath = do
  getglobal "package"
  getfield (-1) "path"
  currentPath <- peek (-1)
  pop 1
  push $ intercalate ";" [currentPath, factorioDataPath]
  setfield (-2) "path"
  pop 1

require :: String -> Lua ()
require luaModule =
  let requireStatement = "require(" ++ (show luaModule) ++ ")"
  in do
    dostring requireStatement
    return ()

loadFactorioLualib :: Lua ()
loadFactorioLualib = require "core.lualib.dataloader"

loadFactorioCoreData :: Lua ()
loadFactorioCoreData = require "core.data"

loadFactorioBaseData :: Lua ()
loadFactorioBaseData = do
  require "data.data"
  require "data.data-updates"

loadFactorioData :: Lua ()
loadFactorioData = do
  openlibs
  addFactorioDataPath
  loadFactorioLualib
  loadFactorioCoreData
  loadFactorioBaseData
  getglobal "print"
  getglobal "data"
  getfield (-1) "raw"
  remove (-2)
  call 1 0
