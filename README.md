# Module Documentation

## Module Data.JSON

### Types

    data Value where
      Object :: M.Map String Value -> Value
      Array :: [Value] -> Value
      String :: String -> Value
      Number :: Number -> Value
      Bool :: Boolean -> Value
      Null :: Value


### Type Class Instances

    instance showValue :: Show Value


### Values

    jsonToValue :: String -> Either String Value