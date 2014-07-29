module Data.JSON 
    ( Value(..), Object(..), Array(..), Parser(..)
    , FromJSON, parseJSON, fail
    , decode, eitherDecode
    , (.:), (.:?), (.!=)

    , ToJSON, toJSON, encode
    , Pair(..), (.=), object
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

--------------------------------------------------------------------------------

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

fail :: forall a. String -> Parser a
fail = Left

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
    parseJSON i        = fail $ show i ++ " is not Boolean."

instance numberFromJSON :: FromJSON Number where
    parseJSON (Number n) = return n
    parseJSON i          = fail $ show i ++ " is not Number."

instance unitFromJSON :: FromJSON Unit where
    parseJSON Null = return unit
    parseJSON i    = fail $ show i ++ " is not Null."

instance stringFromJSON :: FromJSON String where
    parseJSON (String s) = return s
    parseJSON i          = fail $ show i ++ " is not String."

instance arrayFromJSON :: (FromJSON a) => FromJSON [a] where
    parseJSON (Array a) = sequence $ parseJSON <$> a
    parseJSON i         = fail $ show i ++ " is not [a]."

instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
    parseJSON (Array [a,b]) = Tuple <$> parseJSON a <*> parseJSON b
    parseJSON i             = fail $ show i ++ " is not (a,b)."

instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (Object obj) = case M.toList obj of
        [Tuple "Right" r] -> Right <$> parseJSON r
        [Tuple "Left"  l] -> Left  <$> parseJSON l
        _                 -> fail $ show obj ++ " is not (Either a b)."
    parseJSON i = fail $ show i ++ " is not (Either a b)."

instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a) where
    parseJSON a = return $ case parseJSON a of
        Left  _ -> Nothing
        Right r -> Just r

instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a) where
    parseJSON a = S.fromList <$> parseJSON a

instance mapFromJSON :: (Ord a, FromJSON a) => FromJSON (M.Map String a) where
    parseJSON (Object o) = M.fromList <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k v) = case parseJSON v of
            Right r -> return (Tuple k r)
            Left  l -> fail l

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
    \}" :: JSON -> Either String Value

jsonToValue :: String -> Either String Value
jsonToValue = runFn3 jsonParseImpl Left jsonToValueImpl

--------------------------------------------------------------------------------

class ToJSON a where
    toJSON :: a -> Value

type Pair = Tuple String Value

(.=) :: forall a. (ToJSON a) => String -> a -> Pair
(.=) name value = Tuple name (toJSON value)

object :: [Pair] -> Value
object ps = Object $ M.fromList ps

encode :: forall a. (ToJSON a) => a -> String
encode a = valueToString $ toJSON a

instance boolToJSON :: ToJSON Boolean where
    toJSON = Bool

instance numberToJSON :: ToJSON Number where
    toJSON = Number

instance stringToJSON :: ToJSON String where
    toJSON = String

instance unitToJSON :: ToJSON Unit where
    toJSON _ = Null

instance arrayToJSON :: (ToJSON a) => ToJSON [a] where
    toJSON a = Array $ toJSON <$> a

instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Right r) = object ["Right" .= r]
    toJSON (Left  l) = object ["Left"  .= l]

instance mapToJSON :: (ToJSON a) => ToJSON (M.Map String a) where
    toJSON m = Object $ M.map toJSON m

instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a) where
    toJSON Nothing  = Null
    toJSON (Just a) = toJSON a

instance setToJSON :: (ToJSON a) => ToJSON (S.Set a) where
    toJSON s = Array $ toJSON <$> S.toList s

instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b) where
    toJSON (Tuple a b) = Array [toJSON a, toJSON b]

instance valueToJSON :: ToJSON Value where
    toJSON = id

foreign import jsNull "var jsNull = null" :: JSON
foreign import unsafeCoerce "function unsafeCoerce (a) {return a;}"
    :: forall a b. a -> b

foreign import objToHash
    "function objToHash (obj) {\
    \    var hash = {};\
    \    for(var i = 0; i < obj.length; i++) {\
    \        hash[Data_Tuple.fst(obj[i])] = valueToJSONImpl(Data_Tuple.snd(obj[i]));\
    \    }\
    \    return hash;\
    \}" :: [Tuple String Value] -> JSON

valueToJSONImpl :: Value -> JSON
valueToJSONImpl (Object o) = objToHash $ M.toList o
valueToJSONImpl (Array  a) = unsafeCoerce $ valueToJSONImpl <$> a
valueToJSONImpl (String s) = unsafeCoerce s
valueToJSONImpl (Number n) = unsafeCoerce n
valueToJSONImpl (Bool   b) = unsafeCoerce b
valueToJSONImpl Null       = jsNull

foreign import jsonStringify
    "function jsonStringify(json) {\
    \    return JSON.stringify(json);\
    \}" :: JSON -> String

valueToString :: Value -> String
valueToString v = jsonStringify $ valueToJSONImpl v
