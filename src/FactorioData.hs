{-# LANGUAGE DeriveGeneric #-}

module FactorioData where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, typeMismatch, modifyFailure)
import Data.Ratio
import Data.Text hiding (map, filter, concatMap, empty)
import Data.Text.Encoding (encodeUtf8)
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, toRealFloat, toBoundedInteger)
import GHC.Generics (Generic)

data FactorioData = FactorioData
    { icons                 :: HashMap Text Text
    , recipes               :: [Recipe]
    , assemblingMachines    :: [AssemblingMachine]
    , technologies          :: [Technology]
    , resources             :: [Resource]
    , fluids                :: [Fluid]
    } deriving (Generic)

instance FromJSON FactorioData where
    parseJSON = withObject "data" $ \obj -> do
        is  <- getIcons obj
        rs  <- protosForType obj "recipe"
        ams <- protosForType obj "assembling-machine"
        ts  <- protosForType obj "technology"
        res <- protosForType obj "resource"
        fs  <- protosForType obj "fluid"
        return (FactorioData is rs ams ts res fs)

instance ToJSON FactorioData

protosForType :: (FromJSON a) => Object -> Text -> Parser [a]
protosForType allData typeName = do
    protosByName <- withObject (unpack typeName ++ " protos")
                               return
                               (allData ! typeName)
    sequence $ parseJSON <$> filter isEnabled (elems protosByName)

isEnabled :: Value -> Bool
isEnabled (Object obj) = isNotDisabled && noHiddenFlag
  where
    isNotDisabled = case lookup "enabled" obj of
        Just (Bool False) -> False
        _                 -> True

    noHiddenFlag = notElem (String "hidden") $ case lookup "flags" obj of
        Just (Array fs) -> fs
        _               -> V.empty
isEnabled _ = True

getIcons :: Object -> Parser (HashMap Text Text)
getIcons obj = do
    protosByType <- sequence $ withObject "protos by type" return <$> elems obj
    protos       <-
        sequence $ withObject "proto" return <$> concatMap elems protosByType
    iconPairs <- mapM getIconPair . filter hasIcon $ protos
    return $ fromList iconPairs

hasIcon :: Object -> Bool
hasIcon proto = case lookup "icon" proto of
    Just (String _) -> True
    _               -> False

getIconPair :: Object -> Parser (Text, Text)
getIconPair proto = do
    pn <- modifyFailure (++ " - proto name") $ parseJSON $ proto ! "name"
    pi <- modifyFailure (++ " - proto icon") $ parseJSON $ proto ! "icon"
    return (pn, pi)

data Recipe = Recipe
    { recipeName        :: Text
    , recipeCategory    :: Text
    , normal            :: RecipeInfo
    , expensive         :: Maybe RecipeInfo
    } deriving (Generic)

instance FromJSON Recipe where
    parseJSON = withObject "recipe" $ \obj -> do
        rn <- obj .: "name"
        rc <- fromMaybe "basic-crafting" <$> obj .:? "category"
        ei <- obj .:? "expensive"
        ni <- if member "normal" obj
            then obj .: "normal"
            else parseJSON (Object obj)
        return (Recipe rn rc ni ei)

instance ToJSON Recipe

data RecipeInfo = RecipeInfo
    { ingredients   :: [Ingredient]
    , results       :: [RecipeResult]
    , effort        :: Rational
    } deriving (Generic)

instance FromJSON RecipeInfo where
    parseJSON = withObject "recipe info" $ \obj -> do
        is <- obj .: "ingredients"
        rs <- if member "result" obj
            then do
                rn <- obj .: "result"
                return [DeterministicResult rn 1]
            else obj .: "results"
        eff <- fromMaybe 0.5 <$> fmap (fmap convertScientific) (obj .:? "energy_required")
        return (RecipeInfo is rs eff)

instance ToJSON RecipeInfo

data Ingredient = Ingredient Text Int deriving (Generic)

instance FromJSON Ingredient where
    parseJSON (Array arr) = do
        name <- parseJSON ((V.!) arr 0)
        ic <- parsePossiblyStringyInt ((V.!) arr 1)
        return $ Ingredient name ic
    parseJSON (Object obj) = do
        name <- obj .: "name"
        ic <- obj .: "amount"
        return $ Ingredient name ic
    parseJSON v = typeMismatch "array or object" v

instance ToJSON Ingredient

data RecipeResult   = DeterministicResult Text Int
                    | ProbabilisticResult Text Int Rational
                    deriving (Generic)

instance FromJSON RecipeResult where
    parseJSON (Array arr) = do
        rn <- parseJSON ((V.!) arr 0)
        rc <- parsePossiblyStringyInt ((V.!) arr 1)
        return (DeterministicResult rn rc)
    parseJSON (Object obj) = do
        rn <- obj .: "name"
        rc <- parsePossiblyStringyInt (obj ! "amount")
        rp <- fmap (fmap convertScientific) (obj .:? "probability")
        return $ case rp of
            Just p  -> ProbabilisticResult rn rc p
            Nothing -> DeterministicResult rn rc
    parseJSON v = typeMismatch "array or object" v

instance ToJSON RecipeResult

data AssemblingMachine = AssemblingMachine
    { assemblerName :: Text
    , craftingSpeed :: Rational
    , craftingCategories :: [Text]
    } deriving (Generic)

instance FromJSON AssemblingMachine where
    parseJSON = withObject "assembling machine" $ \obj -> do
        an <- obj .: "name"
        cs <- obj .: "crafting_speed" >>= parseRational
        cc <- obj .: "crafting_categories"
        return $ AssemblingMachine an cs cc

instance ToJSON AssemblingMachine

data Technology = Technology
    { technologyName :: Text
    , prerequisites :: [Text]
    , effects :: [TechnologyEffect]
    , researchUnitTime :: Int
    , researchUnitCount :: Int
    , researchUnitIngredients :: [Ingredient]
    } deriving (Generic)

instance FromJSON Technology where
    parseJSON = withObject "technology" $ \obj -> do
        tn <- obj .: "name"
        let withTechInfo = modifyFailure (++ " - tech " ++ unpack tn)
        pr <- withTechInfo $ fromMaybe [] <$> obj .:? "prerequisites"
        es <- withTechInfo $ fromMaybe [] <$> obj .:? "effects"
        (ut, uc, uis) <- withTechInfo $ parseTechnologyUnit (obj ! "unit")
        return $ Technology tn pr es ut uc uis

instance ToJSON Technology

parseTechnologyUnit :: Value -> Parser (Int, Int, [Ingredient])
parseTechnologyUnit = withObject "technology unit" $ \obj -> do
    ut <- parsePossiblyStringyInt (obj ! "time")

    -- count might be a stringified int...eg mining-productivity-1
    -- might be helpful to do this check generically e_e
    -- also, the default val here is for stuff that has a "count_formula", eg demo-productivity-1
    uc <- maybe (return 0) parsePossiblyStringyInt (lookup "count" obj)
    uis <- obj .: "ingredients"
    return (ut, uc, uis)

data TechnologyEffect   = ModifierEffect ModifierTarget Rational
                        | RecipeUnlock Text
                        | EffectToggle Text Bool
                        deriving (Generic)

instance FromJSON TechnologyEffect where
    parseJSON = withObject "technology effect" $ \obj -> do
        et <- (obj .: "type") :: Parser Text
        if et == "unlock-recipe"
            then RecipeUnlock <$> obj .: "recipe"
            else case obj ! "modifier" of
                Number _    -> parseModifierEffect obj
                Bool b      -> return $ EffectToggle et b
                _           -> typeMismatch "number or bool" (obj ! "modifier")

instance ToJSON TechnologyEffect

parseModifierEffect :: Object -> Parser TechnologyEffect
parseModifierEffect obj = do
    mv <- obj .: "modifier" >>= parseRational
    mt <- parseJSON (Object obj)
    return (ModifierEffect mt mv)

data ModifierTarget = GenericModifier Text
                    | AmmoCategoryModifier Text Text
                    deriving (Generic)

instance FromJSON ModifierTarget where
    parseJSON = withObject "modifier effect" $ \obj -> do
        mt <- obj .: "type"
        ac <- obj .:? "ammo_category"
        return $ case ac of
            Just categoryName   -> AmmoCategoryModifier mt categoryName
            Nothing             -> GenericModifier mt

instance ToJSON ModifierTarget

data Fluid = Fluid
    { fluidName :: Text
    , heatCapacity :: Quantity
    , maxTemperature :: Quantity
    , defaultTemperature :: Quantity
    } deriving (Generic)

instance FromJSON Fluid where
    parseJSON = withObject "fluid" $ \obj -> do
        fn <- obj .: "name"
        let withFluidInfo = modifyFailure (++ " - fluid " ++ unpack fn)
        fhc <- withFluidInfo $ obj .: "heat_capacity"
        fmt <- withFluidInfo $ obj .: "max_temperature"
        fdt <- withFluidInfo $ obj .: "default_temperature"
        return $ Fluid fn fhc fmt fdt

instance ToJSON Fluid

data Quantity = Quantity { value :: Rational, unit :: Maybe Text } deriving (Generic)

instance FromJSON Quantity where
    parseJSON val = case val of
        String s -> parseStringQuantity s
        Number n -> return $ Quantity (convertScientific n) Nothing
        _        -> typeMismatch "string or number" val

parseStringQuantity :: Text -> Parser Quantity
parseStringQuantity s = do
    let (sv, u) = span (not . isAlpha) s
    v <- maybe
        (typeMismatch "quantity val" (String s))
        (withScientific "quantity value" (return . convertScientific))
        (decode . fromStrict . encodeUtf8 $ sv)
    return $ Quantity v (Just u)

instance ToJSON Quantity

data Resource = Resource
    { resourceName :: Text
    , miningTime :: Rational
    , requiredFluid :: Maybe MiningFluid
    } deriving (Generic)

instance FromJSON Resource where
    parseJSON = withObject "resource" $ \obj -> do
        rn <- obj .: "name"
        minable <- obj .: "minable"
        mt <- withObject "minable" getMiningTime minable
        let rf = parseMaybe parseJSON minable
        return (Resource rn mt rf)

instance ToJSON Resource

getMiningTime :: Object -> Parser Rational
getMiningTime obj = obj .: "mining_time" >>= parseRational

data MiningFluid = MiningFluid Text Rational deriving (Generic)

instance FromJSON MiningFluid where
    parseJSON = withObject "minable" $ \obj -> do
        rf <- obj .: "required_fluid"
        fa <- obj .: "fluid_amount" >>= parseRational
        return (MiningFluid rf fa)

instance ToJSON MiningFluid

parseRational :: Value -> Parser Rational
parseRational = withScientific "rational" (return . convertScientific)

convertScientific :: Scientific -> Rational
convertScientific sci = approxRational rf 0.0000001
    where
        rf = toRealFloat sci
        (n, f) = properFraction sci
        frf = toRealFloat f
        err = 10 ** (fromIntegral . floor $ logBase 10 frf) :: Float

parsePossiblyStringyInt :: Value -> Parser Int
parsePossiblyStringyInt v = case v of
    String s    -> return . read . unpack $ s
    Number n    -> return . fromMaybe 0 . toBoundedInteger $ n
    _           -> typeMismatch "string or number" v
