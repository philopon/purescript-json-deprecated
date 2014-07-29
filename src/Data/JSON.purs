module Data.JSON 
    ( Value(..), Object(..), Array(..), Parser(..)
    , decode, eitherDecode
    , (.:), (.:?), (.!=)
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.Function
import Data.Tuple

type Object = M.Map String Value
type Array  = [Value]
type Parser = Either String

data Value
    = Object Object
    | Array  Array
    | String String
    | Number Number
    | Bool   Boolean
    | Null

instance showValue :: Show Value where
    show (Object m) = "Object " ++ show m
    show (Array vs) = "Array "  ++ show vs
    show (String s) = "String " ++ show s
    show (Number n) = "Number " ++ show n
    show (Bool   b) = "Bool "   ++ show b
    show Null       = "Null"

instance eqValue :: Eq Value where
    (==) (Object a) (Object b) = a == b
    (==) (Array  a) (Array  b) = a == b
    (==) (String a) (String b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (Bool   a) (Bool   b) = a == b
    (==) Null       Null       = true
    (==) _          _          = false
    (/=) a          b          = not (a == b)

class FromJSON a where
    parseJSON :: Value -> Parser a

eitherDecode :: forall a. (FromJSON a) => String -> Either String a
eitherDecode s = do
    v <- jsonToValue s
    parseJSON v

decode :: forall a. (FromJSON a) => String -> Maybe a
decode s = case eitherDecode s of
    Right a -> Just a
    Left  _ -> Nothing

sequence :: forall m a. (Monad m) => [m a] -> m [a]
sequence [] = return []
sequence (a:as) = do
    a' <- a
    as' <- sequence as
    return (a' : as')

instance valueFromJSON :: FromJSON Value where
    parseJSON = Right

instance boolFromJSON :: FromJSON Boolean where
    parseJSON (Bool b) = Right b
    parseJSON i        = Left $ show i ++ " is not Boolean."

instance numberFromJSON :: FromJSON Number where
    parseJSON (Number n) = Right n
    parseJSON i          = Left $ show i ++ " is not Number."

instance unitFromJSON :: FromJSON Unit where
    parseJSON Null = Right unit
    parseJSON i    = Left $ show i ++ " is not Null."

instance stringFromJSON :: FromJSON String where
    parseJSON (String s) = Right s
    parseJSON i          = Left $ show i ++ " is not String."

instance arrayFromJSON :: (FromJSON a) => FromJSON [a] where
    parseJSON (Array a) = sequence $ parseJSON <$> a
    parseJSON i          = Left $ show i ++ " is not [a]."

instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
    parseJSON (Array [a,b]) = Tuple <$> parseJSON a <*> parseJSON b
    parseJSON i             = Left $ show i ++ " is not (a,b)."

instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (Object obj) = case obj .: "Right" of
        Right r -> Right (Right r)
        Left  _ -> case obj .: "Left" of
            Right l -> Right (Left l)
            Left  _ -> Left $ show obj ++ " is not (Either a b)."
    parseJSON i = Left $ show i ++ " is not (Either a b)."

instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a) where
    parseJSON a = Right $ case parseJSON a of
        Left  _ -> Nothing
        Right r -> Just r

instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a) where
    parseJSON a = S.fromList <$> parseJSON a

instance mapFromJSON :: (Ord a, FromJSON a) => FromJSON (M.Map String a) where
    parseJSON (Object o) = M.fromList <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k v) = case parseJSON v of
            Right r -> Right (Tuple k r)
            Left  l -> Left l

(.:) :: forall a. (FromJSON a) => Object -> String -> Parser a
(.:) obj key = case M.lookup key obj of
    Nothing -> Left $ "key " ++ show key ++ " not present"
    Just v  -> parseJSON v

(.:?) :: forall a. (FromJSON a) => Object -> String -> Parser (Maybe a)
(.:?) obj key = case M.lookup key obj of
    Nothing -> return Nothing
    Just v  -> parseJSON v

(.!=) :: forall a. Parser (Maybe a) -> a -> Parser a
(.!=) pmval val = fromMaybe val <$> pmval

foreign import data JSON :: *

foreign import jsonParseImpl
    "function jsonParseImpl (left, right, string) {\
    \    try       { return right(JSON.parse(string)); }\
    \    catch (e) { return left(e.toString()); }\
    \}" :: forall a. Fn3 (String -> a) (JSON -> a) String a

jsonParse :: String -> Either String JSON
jsonParse = runFn3 jsonParseImpl Left Right

foreign import jsonToValueImpl
    "function jsonToValueImpl (json) {\
    \    var typ    = Object.prototype.toString.call(json).slice(8,-1); \
    \    var right = Data_Either.Right; \
    \    var left  = Data_Either.Left; \
    \    if        (typ === 'Number') { \
    \        return right(Number(json));\
    \\
    \    } else if (typ === 'Boolean') { \
    \        return right(Bool(json));\
    \\
    \    } else if (typ === 'String') { \
    \        return right(String(json));\
    \\
    \    } else if (typ === 'Null') { \
    \        return right(Null);\
    \\
    \    } else if (typ === 'Array') { \
    \        var ary = [];\
    \        for(var i = 0; i < json.length; i++) { \
    \            Data_Either.either \
    \                (function(l){return left(l)}) \
    \                (function(r){ary.push(r)}) \
    \                (jsonToValueImpl(json[i])) \
    \        } \
    \        return right(Array(ary));\
    \\
    \    } else if (typ === 'Object') {\
    \        var insert = Data_Function.mkFn3(Data_Map.insert(Prelude.ordString()));\
    \        var obj = Data_Map.empty;\
    \        for(var k in json) { \
    \            Data_Either.either \
    \                (function(l){return left(l)}) \
    \                (function(r){obj = insert(k, r, obj)}) \
    \                (jsonToValueImpl(json[k]));\
    \        } \
    \        return right(Object(obj));\
    \\
    \    } else { \
    \        return left('unknown type: ' + typ);\
    \    }\
    \}" :: forall a. JSON -> Either String Value

jsonToValue :: String -> Either String Value
jsonToValue = runFn3 jsonParseImpl Left jsonToValueImpl
