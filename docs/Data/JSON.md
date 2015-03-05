# Module Documentation

## Module Data.JSON

#### `JObject`

``` purescript
type JObject = M.Map String JValue
```


#### `JArray`

``` purescript
type JArray = [JValue]
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


#### `showValue`

``` purescript
instance showValue :: Show JValue
```


#### `eqValue`

``` purescript
instance eqValue :: Eq JValue
```


#### `FromJSON`

``` purescript
class FromJSON a where
  parseJSON :: JValue -> JParser a
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


#### `valueFromJSON`

``` purescript
instance valueFromJSON :: FromJSON JValue
```


#### `boolFromJSON`

``` purescript
instance boolFromJSON :: FromJSON Boolean
```


#### `numberFromJSON`

``` purescript
instance numberFromJSON :: FromJSON Number
```


#### `unitFromJSON`

``` purescript
instance unitFromJSON :: FromJSON Unit
```


#### `stringFromJSON`

``` purescript
instance stringFromJSON :: FromJSON String
```


#### `arrayFromJSON`

``` purescript
instance arrayFromJSON :: (FromJSON a) => FromJSON [a]
```


#### `tupleFromJSON`

``` purescript
instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b)
```


#### `eitherFromJSON`

``` purescript
instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b)
```


#### `maybeFromJSON`

``` purescript
instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a)
```


#### `setFromJSON`

``` purescript
instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a)
```


#### `mapFromJSON`

``` purescript
instance mapFromJSON :: (FromJSON a) => FromJSON (M.Map String a)
```


#### `(.:)`

``` purescript
(.:) :: forall a. (FromJSON a) => JObject -> String -> JParser a
```


#### `(.:?)`

``` purescript
(.:?) :: forall a. (FromJSON a) => JObject -> String -> JParser (Maybe a)
```


#### `(.!=)`

``` purescript
(.!=) :: forall a. JParser (Maybe a) -> a -> JParser a
```


#### `ToJSON`

``` purescript
class ToJSON a where
  toJSON :: a -> JValue
```

#### `Pair`

``` purescript
type Pair = Tuple String JValue
```


#### `(.=)`

``` purescript
(.=) :: forall a. (ToJSON a) => String -> a -> Pair
```


#### `object`

``` purescript
object :: [Pair] -> JValue
```


#### `encode`

``` purescript
encode :: forall a. (ToJSON a) => a -> String
```


#### `boolToJSON`

``` purescript
instance boolToJSON :: ToJSON Boolean
```


#### `numberToJSON`

``` purescript
instance numberToJSON :: ToJSON Number
```


#### `stringToJSON`

``` purescript
instance stringToJSON :: ToJSON String
```


#### `unitToJSON`

``` purescript
instance unitToJSON :: ToJSON Unit
```


#### `arrayToJSON`

``` purescript
instance arrayToJSON :: (ToJSON a) => ToJSON [a]
```


#### `eitherToJSON`

``` purescript
instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b)
```


#### `mapToJSON`

``` purescript
instance mapToJSON :: (ToJSON a) => ToJSON (M.Map String a)
```


#### `maybeToJSON`

``` purescript
instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a)
```


#### `setToJSON`

``` purescript
instance setToJSON :: (ToJSON a) => ToJSON (S.Set a)
```


#### `tupleToJSON`

``` purescript
instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b)
```


#### `valueToJSON`

``` purescript
instance valueToJSON :: ToJSON JValue
```




