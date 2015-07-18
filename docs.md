## Module Data.JSON

#### `JObject`

``` purescript
type JObject = Map String JValue
```

#### `JArray`

``` purescript
type JArray = Array JValue
```

#### `JParser`

``` purescript
type JParser = Either String
```

#### `JValue`

``` purescript
data JValue
  = JObject JObject
  | JArray JArray
  | JString String
  | JNumber Number
  | JBool Boolean
  | JNull
```

##### Instances
``` purescript
instance showValue :: Show JValue
instance eqValue :: Eq JValue
instance valueFromJSON :: FromJSON JValue
instance valueToJSON :: ToJSON JValue
```

#### `FromJSON`

``` purescript
class FromJSON a where
  parseJSON :: JValue -> JParser a
```

##### Instances
``` purescript
instance valueFromJSON :: FromJSON JValue
instance boolFromJSON :: FromJSON Boolean
instance numberFromJSON :: FromJSON Number
instance unitFromJSON :: FromJSON Unit
instance stringFromJSON :: FromJSON String
instance arrayFromJSON :: (FromJSON a) => FromJSON (Array a)
instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b)
instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b)
instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a)
instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (Set a)
instance mapFromJSON :: (FromJSON a) => FromJSON (Map String a)
```

#### `eitherDecode`

``` purescript
eitherDecode :: forall a. (FromJSON a) => String -> Either String a
```

#### `decode`

``` purescript
decode :: forall a. (FromJSON a) => String -> Maybe a
```

#### `fail`

``` purescript
fail :: forall a. String -> JParser a
```

#### `(.:)`

``` purescript
(.:) :: forall a. (FromJSON a) => JObject -> String -> JParser a
```

_left-associative / precedence -1_

#### `(.:?)`

``` purescript
(.:?) :: forall a. (FromJSON a) => JObject -> String -> JParser (Maybe a)
```

_left-associative / precedence -1_

#### `(.!=)`

``` purescript
(.!=) :: forall a. JParser (Maybe a) -> a -> JParser a
```

_left-associative / precedence -1_

#### `ToJSON`

``` purescript
class ToJSON a where
  toJSON :: a -> JValue
```

##### Instances
``` purescript
instance boolToJSON :: ToJSON Boolean
instance numberToJSON :: ToJSON Number
instance stringToJSON :: ToJSON String
instance unitToJSON :: ToJSON Unit
instance arrayToJSON :: (ToJSON a) => ToJSON (Array a)
instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b)
instance mapToJSON :: (ToJSON a) => ToJSON (Map String a)
instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a)
instance setToJSON :: (ToJSON a) => ToJSON (Set a)
instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b)
instance valueToJSON :: ToJSON JValue
```

#### `Pair`

``` purescript
type Pair = Tuple String JValue
```

#### `(.=)`

``` purescript
(.=) :: forall a. (ToJSON a) => String -> a -> Pair
```

_left-associative / precedence -1_

#### `object`

``` purescript
object :: Array Pair -> JValue
```

#### `encode`

``` purescript
encode :: forall a. (ToJSON a) => a -> String
```



