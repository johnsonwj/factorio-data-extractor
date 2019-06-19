module FactorioData where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, typeMismatch)
import Data.Ratio
import Data.Text hiding (map, filter, concatMap, empty)
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

type WidgetName = Text

data FactorioData = FactorioData
    { icons :: HashMap Text Text
    , recipes :: [Recipe]
    , technologies :: [Technology]
    , resources :: [Resource]
    , fluids :: [Fluid]
    }

instance FromJSON FactorioData where
    parseJSON = withObject "data" $ \obj -> do
        -- expected Text, encountered Object?
        is <- getIcons obj

        -- todo: these need to unpack protos-by-type into lists of protos
        rs <- obj .: "recipe"
        ts <- obj .: "technology"
        res <- obj .: "resource"
        fs <- obj .: "fluid"
        return (FactorioData is rs ts res fs)

getIcons :: Object -> Parser (HashMap Text Text)
getIcons obj = do
    protosByType <- sequence $ withObject "protos by type" return <$> elems obj
    protos <- sequence $ withObject "proto" return <$> concatMap elems protosByType
    iconPairs <- mapM getIconPair . filter hasIcon $ protos
    return $ fromList iconPairs

hasIcon :: Object -> Bool
hasIcon = member "icon"

getIconPair :: Object -> Parser (Text, Text)
getIconPair proto = do
    pn <- parseJSON $ proto ! "name"
    pi <- parseJSON $ proto ! "icon"
    return (pn, pi)

data Recipe = Recipe
    { recipeName    :: Text
    , normal        :: RecipeInfo
    , expensive     :: Maybe RecipeInfo
    }

instance FromJSON Recipe where
    parseJSON = withObject "recipe" $ \obj -> do
        rn <- obj .: "name"
        ei <- obj .:? "expensive"
        ni <- if member "normal" obj
            then obj .: "normal"
            else parseJSON (Object obj)
        return (Recipe rn ni ei)

data RecipeInfo = RecipeInfo
    { ingredients   :: [Ingredient]
    , results       :: [RecipeResult]
    , effort        :: Rational
    }

instance FromJSON RecipeInfo where
    parseJSON = withObject "recipe info" $ \obj -> do
        is <- obj .: "ingredients"
        rs <- if member "result" obj
            then do
                rn <- obj .: "result"
                return [DeterministicResult rn 1]
            else obj .: "results"
        eff <- fromMaybe 0.5 <$> (obj .:? "energy_required")
        return (RecipeInfo is rs eff)

data Ingredient = Ingredient Text Int

instance FromJSON Ingredient where
    parseJSON (Array arr) = do
        name <- parseJSON ((V.!) arr 0)
        ic <- parseJSON ((V.!) arr 1)
        return $ Ingredient name ic
    parseJSON (Object obj) = do
        name <- obj .: "name"
        ic <- obj .: "amount"
        return $ Ingredient name ic
    parseJSON v = typeMismatch "array or object" v

data RecipeResult   = DeterministicResult Text Int
                    | ProbabilisticResult Text Int Rational

instance FromJSON RecipeResult where
    parseJSON (Array arr) = do
        rn <- parseJSON ((V.!) arr 0)
        rc <- parseJSON ((V.!) arr 1)
        return (DeterministicResult rn rc)
    parseJSON (Object obj) = do
        rn <- obj .: "name"
        rc <- obj .: "amount"
        rp <- obj .:? "probability"
        return $ case rp of
            Just p  -> ProbabilisticResult rn rc p
            Nothing -> DeterministicResult rn rc
    parseJSON v = typeMismatch "array or object" v
                                            
data Technology = Technology
    { technologyName :: Text
    , prerequisites :: [Text]
    , effects :: [TechnologyEffect]
    , researchUnitTime :: Int
    , researchUnitCount :: Int
    , researchUnitIngredients :: [Ingredient]
    }

instance FromJSON Technology where
    parseJSON = withObject "technology" $ \obj -> do
        tn <- obj .: "name"
        pr <- obj .: "prerequisites"
        es <- obj .: "effects"
        (ut, uc, uis) <- parseTechnologyUnit (obj ! "unit")
        return $ Technology tn pr es ut uc uis

parseTechnologyUnit :: Value -> Parser (Int, Int, [Ingredient])
parseTechnologyUnit = withObject "technology unit" $ \obj -> do
    ut <- obj .: "time"
    uc <- obj .: "count"
    uis <- obj .: "ingredients"
    return (ut, uc, uis)

data TechnologyEffect   = ModifierEffect ModifierTarget Rational
                        | RecipeUnlock Text

instance FromJSON TechnologyEffect where
    parseJSON = withObject "technology effect" $ \obj -> do
        et <- (obj .: "type") :: Parser Text
        if et == "unlock-recipe"
            then RecipeUnlock <$> obj .: "recipe"
            else do
                mv <- obj .: "modifier"
                mt <- parseJSON (Object obj)
                return (ModifierEffect mt mv)



data ModifierTarget = GenericModifier Text
                    | AmmoCategoryModifier Text Text

instance FromJSON ModifierTarget where
    parseJSON = withObject "modifier effect" $ \obj -> do
        mt <- obj .: "type"
        ac <- obj .:? "ammo_category"
        return $ case ac of
            Just categoryName   -> AmmoCategoryModifier mt categoryName
            Nothing             -> GenericModifier mt

data Fluid = Fluid
    { fluidName :: Text
    , heatCapacity :: Rational
    , maxTemperature :: Rational
    , defaultTemperature :: Rational
    }

instance FromJSON Fluid where
    parseJSON = withObject "fluid" $ \obj -> do
        fn <- obj .: "name"
        let fhc = 0
        fmt <- obj .: "max_temperature"
        fdt <- obj .: "default_temperature"
        return $ Fluid fn fhc fmt fdt

data Resource = Resource
    { resourceName :: Text
    , miningTime :: Rational
    , requiredFluid :: Maybe MiningFluid
    }

instance FromJSON Resource where
    parseJSON = withObject "resource" $ \obj -> do
        rn <- obj .: "name"
        minable <- obj .: "minable"
        mt <- withObject "minable" getMiningTime minable
        let rf = parseMaybe parseJSON minable
        return (Resource rn mt rf)

getMiningTime :: Object -> Parser Rational
getMiningTime obj = obj .: "mining_time"

data MiningFluid = MiningFluid Text Rational

instance FromJSON MiningFluid where
    parseJSON = withObject "minable" $ \obj -> do
        rf <- obj .: "required_fluid"
        fa <- obj .: "fluid_amount"
        return (MiningFluid rf fa)