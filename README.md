purescript-json [![Build Status](https://travis-ci.org/philopon/purescript-json.svg?branch=master)](https://travis-ci.org/philopon/purescript-json)
===
JSON parsing library like aeson for purescript.

# Module Documentation

## Module Data.JSON

### Types

    type Array  = [Value]

    type Object  = M.Map String Value

    type Pair  = Tuple String Value

    type Parser  = Either String

    data Value where
      Object :: Object -> Value
      Array :: Array -> Value
      String :: String -> Value
      Number :: Number -> Value
      Bool :: Boolean -> Value
      Null :: Value


### Type Classes

    class FromJSON a where
      parseJSON :: Value -> Parser a

    class ToJSON a where
      toJSON :: a -> Value


### Type Class Instances

    instance arrayFromJSON :: (FromJSON a) => FromJSON [a]

    instance arrayToJSON :: (ToJSON a) => ToJSON [a]

    instance boolFromJSON :: FromJSON Prim.Boolean

    instance boolToJSON :: ToJSON Prim.Boolean

    instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b)

    instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b)

    instance eqValue :: Eq Value

    instance mapFromJSON :: (Ord a, FromJSON a) => FromJSON (M.Map Prim.String a)

    instance mapToJSON :: (ToJSON a) => ToJSON (M.Map Prim.String a)

    instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a)

    instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a)

    instance numberFromJSON :: FromJSON Prim.Number

    instance numberToJSON :: ToJSON Prim.Number

    instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a)

    instance setToJSON :: (ToJSON a) => ToJSON (S.Set a)

    instance showValue :: Show Value

    instance stringFromJSON :: FromJSON Prim.String

    instance stringToJSON :: ToJSON Prim.String

    instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b)

    instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b)

    instance unitFromJSON :: FromJSON Unit

    instance unitToJSON :: ToJSON Unit

    instance valueFromJSON :: FromJSON Value

    instance valueToJSON :: ToJSON Value


### Values

    (.!=) :: forall a. Parser (Maybe a) -> a -> Parser a

    (.:) :: forall a. (FromJSON a) => Object -> String -> Parser a

    (.:?) :: forall a. (FromJSON a) => Object -> String -> Parser (Maybe a)

    (.=) :: forall a. (ToJSON a) => String -> a -> Pair

    decode :: forall a. (FromJSON a) => String -> Maybe a

    eitherDecode :: forall a. (FromJSON a) => String -> Either String a

    encode :: forall a. (ToJSON a) => a -> String

    fail :: forall a. String -> Parser a

    object :: [Pair] -> Value