# Module Documentation

## Module Data.JSON

### Types

    type Array  = [Value]

    type Object  = M.Map String Value

    type Parser  = Either String

    data Value where
      Object :: Object -> Value
      Array :: Array -> Value
      String :: String -> Value
      Number :: Number -> Value
      Bool :: Boolean -> Value
      Null :: Value


### Type Classes


### Type Class Instances

    instance arrayFromJSON :: (FromJSON a) => FromJSON [a]

    instance boolFromJSON :: FromJSON Prim.Boolean

    instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b)

    instance mapFromJSON :: (Ord a, FromJSON a) => FromJSON (M.Map Prim.String a)

    instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a)

    instance numberFromJSON :: FromJSON Prim.Number

    instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a)

    instance showValue :: Show Value

    instance stringFromJSON :: FromJSON Prim.String

    instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b)

    instance unitFromJSON :: FromJSON Unit

    instance valueFromJSON :: FromJSON Value


### Values

    (.!=) :: forall a. Parser (Maybe a) -> a -> Parser a

    (.:) :: forall a. (FromJSON a) => Object -> String -> Parser a

    (.:?) :: forall a. (FromJSON a) => Object -> String -> Parser (Maybe a)

    decode :: forall a. (FromJSON a) => String -> Maybe a

    eitherDecode :: forall a. (FromJSON a) => String -> Either String a