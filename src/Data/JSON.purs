module Data.JSON
    ( JValue(..), JObject(..), JArray(..), JParser(..)
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

type JObject = M.Map String JValue
type JArray  = [JValue]
type JParser = Either String

data JValue
    = JObject JObject
    | JArray  JArray
    | JString String
    | JNumber Number
    | JBool   Boolean
    | JNull

instance showValue :: Show JValue where
    show (JObject m) = "JObject " ++ show m
    show (JArray vs) = "JArray "  ++ show vs
    show (JString s) = "JString " ++ show s
    show (JNumber n) = "JNumber " ++ show n
    show (JBool   b) = "JBool "   ++ show b
    show JNull       = "JNull"

instance eqValue :: Eq JValue where
    (==) (JObject a) (JObject b) = a == b
    (==) (JArray  a) (JArray  b) = a == b
    (==) (JString a) (JString b) = a == b
    (==) (JNumber a) (JNumber b) = a == b
    (==) (JBool   a) (JBool   b) = a == b
    (==) JNull       JNull       = true
    (==) _          _            = false
    (/=) a          b            = not (a == b)

--------------------------------------------------------------------------------

class FromJSON a where
    parseJSON :: JValue -> JParser a

eitherDecode :: forall a. (FromJSON a) => String -> Either String a
eitherDecode s = do
    v <- jsonToValue s
    parseJSON v

decode :: forall a. (FromJSON a) => String -> Maybe a
decode s = case eitherDecode s of
    Right a -> Just a
    Left  _ -> Nothing

fail :: forall a. String -> JParser a
fail = Left

sequence :: forall m a. (Monad m) => [m a] -> m [a]
sequence [] = return []
sequence (a:as) = do
    a' <- a
    as' <- sequence as
    return (a' : as')

instance valueFromJSON :: FromJSON JValue where
    parseJSON = Right

instance boolFromJSON :: FromJSON Boolean where
    parseJSON (JBool b) = Right b
    parseJSON i         = fail $ show i ++ " is not Boolean."

instance numberFromJSON :: FromJSON Number where
    parseJSON (JNumber n) = return n
    parseJSON i           = fail $ show i ++ " is not Number."

instance unitFromJSON :: FromJSON Unit where
    parseJSON JNull = return unit
    parseJSON i     = fail $ show i ++ " is not Null."

instance stringFromJSON :: FromJSON String where
    parseJSON (JString s) = return s
    parseJSON i          = fail $ show i ++ " is not String."

instance arrayFromJSON :: (FromJSON a) => FromJSON [a] where
    parseJSON (JArray a) = sequence $ parseJSON <$> a
    parseJSON i          = fail $ show i ++ " is not [a]."

instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
    parseJSON (JArray [a,b]) = Tuple <$> parseJSON a <*> parseJSON b
    parseJSON i              = fail $ show i ++ " is not (a,b)."

instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (JObject obj) = case M.toList obj of
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
    parseJSON (JObject o) = M.fromList <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k v) = case parseJSON v of
            Right r -> return (Tuple k r)
            Left  l -> fail l

(.:) :: forall a. (FromJSON a) => JObject -> String -> JParser a
(.:) obj key = case M.lookup key obj of
    Nothing -> Left $ "key " ++ show key ++ " not present"
    Just v  -> parseJSON v

(.:?) :: forall a. (FromJSON a) => JObject -> String -> JParser (Maybe a)
(.:?) obj key = case M.lookup key obj of
    Nothing -> return Nothing
    Just v  -> parseJSON v

(.!=) :: forall a. JParser (Maybe a) -> a -> JParser a
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
    toJSON :: a -> JValue

type Pair = Tuple String JValue

(.=) :: forall a. (ToJSON a) => String -> a -> Pair
(.=) name value = Tuple name (toJSON value)

object :: [Pair] -> JValue
object ps = JObject $ M.fromList ps

encode :: forall a. (ToJSON a) => a -> String
encode a = valueToString $ toJSON a

instance boolToJSON :: ToJSON Boolean where
    toJSON = JBool

instance numberToJSON :: ToJSON Number where
    toJSON = JNumber

instance stringToJSON :: ToJSON String where
    toJSON = JString

instance unitToJSON :: ToJSON Unit where
    toJSON _ = JNull

instance arrayToJSON :: (ToJSON a) => ToJSON [a] where
    toJSON a = JArray $ toJSON <$> a

instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Right r) = object ["Right" .= r]
    toJSON (Left  l) = object ["Left"  .= l]

instance mapToJSON :: (ToJSON a) => ToJSON (M.Map String a) where
    toJSON m = JObject $ M.map toJSON m

instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a) where
    toJSON Nothing  = JNull
    toJSON (Just a) = toJSON a

instance setToJSON :: (ToJSON a) => ToJSON (S.Set a) where
    toJSON s = JArray $ toJSON <$> S.toList s

instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b) where
    toJSON (Tuple a b) = JArray [toJSON a, toJSON b]

instance valueToJSON :: ToJSON JValue where
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
    \}" :: [Tuple String JValue] -> JSON

valueToJSONImpl :: JValue -> JSON
valueToJSONImpl (JObject o) = objToHash $ M.toList o
valueToJSONImpl (JArray  a) = unsafeCoerce $ valueToJSONImpl <$> a
valueToJSONImpl (JString s) = unsafeCoerce s
valueToJSONImpl (JNumber n) = unsafeCoerce n
valueToJSONImpl (JBool   b) = unsafeCoerce b
valueToJSONImpl JNull       = jsNull

foreign import jsonStringify
    "function jsonStringify(json) {\
    \    return JSON.stringify(json);\
    \}" :: JSON -> String

valueToString :: JValue -> String
valueToString v = jsonStringify $ valueToJSONImpl v
