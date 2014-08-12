# Module Documentation

## Module Data.JSON

### Types

    type JArray  = [JValue]

    type JObject  = M.Map String JValue

    type JParser  = Either String

    data JValue where
      JObject :: JObject -> JValue
      JArray :: JArray -> JValue
      JString :: String -> JValue
      JNumber :: Number -> JValue
      JBool :: Boolean -> JValue
      JNull :: JValue

    type Pair  = Tuple String JValue


### Type Classes

    class FromJSON a where
      parseJSON :: JValue -> JParser a

    class ToJSON a where
      toJSON :: a -> JValue


### Type Class Instances

    instance arrayFromJSON :: (FromJSON a) => FromJSON [a]

    instance arrayToJSON :: (ToJSON a) => ToJSON [a]

    instance boolFromJSON :: FromJSON Boolean

    instance boolToJSON :: ToJSON Boolean

    instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b)

    instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b)

    instance eqValue :: Eq JValue

    instance mapFromJSON :: (Ord a, FromJSON a) => FromJSON (M.Map String a)

    instance mapToJSON :: (ToJSON a) => ToJSON (M.Map String a)

    instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a)

    instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a)

    instance numberFromJSON :: FromJSON Number

    instance numberToJSON :: ToJSON Number

    instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a)

    instance setToJSON :: (ToJSON a) => ToJSON (S.Set a)

    instance showValue :: Show JValue

    instance stringFromJSON :: FromJSON String

    instance stringToJSON :: ToJSON String

    instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b)

    instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b)

    instance unitFromJSON :: FromJSON Unit

    instance unitToJSON :: ToJSON Unit

    instance valueFromJSON :: FromJSON JValue

    instance valueToJSON :: ToJSON JValue


### Values

    (.!=) :: forall a. JParser (Maybe a) -> a -> JParser a

    (.:) :: forall a. (FromJSON a) => JObject -> String -> JParser a

    (.:?) :: forall a. (FromJSON a) => JObject -> String -> JParser (Maybe a)

    (.=) :: forall a. (ToJSON a) => String -> a -> Pair

    decode :: forall a. (FromJSON a) => String -> Maybe a

    eitherDecode :: forall a. (FromJSON a) => String -> Either String a

    encode :: forall a. (ToJSON a) => a -> String

    fail :: forall a. String -> JParser a

    object :: [Pair] -> JValue