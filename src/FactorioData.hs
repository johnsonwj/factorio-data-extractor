{-# LANGUAGE DeriveGeneric #-}

module FactorioData where

import Data.Aeson (FromJSON, parseJSON, ToJSON)
import GHC.Generics

data FactorioData = FactorioData
    { recipes   :: [Recipe]
    , items     :: [Item]
    } deriving (Eq, Generic)

instance ToJSON FactorioData

data Recipe = Recipe deriving (Eq, Generic)

instance ToJSON Recipe

data Item = Item deriving (Eq, Generic)

instance ToJSON Item
