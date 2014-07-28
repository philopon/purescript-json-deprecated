module Data.JSON 
    ( Value(..)

    , jsonToValue
    ) where

import qualified Data.Map as M
import Data.Either
import Data.Function

data Value
    = Object (M.Map String Value)
    | Array  [Value]
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
    \        var insert = Data_Function.mkFn3(Data_Map.insert(Prelude.ordString));\
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
