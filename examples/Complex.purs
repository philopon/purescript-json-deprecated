module Examples.Complex where

import Prelude

import Control.Monad.Eff
import qualified Control.Monad.Eff.Console as Console

import Data.Maybe
import Data.Either
import Data.JSON
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

data Foo = Foo Number Number String

instance showFoo :: Show Foo where
    show (Foo a b c) = "Foo " ++ show a ++ " " ++ show b ++ " " ++ show c

instance fooFromJSON :: FromJSON Foo where
    parseJSON (JArray [a,b,c]) =
        Foo <$> parseJSON a <*> parseJSON b <*> parseJSON c
    parseJSON _ = fail "Foo parse failed."

instance fooToJSON :: ToJSON Foo where
    toJSON (Foo a b c) = JArray [toJSON a, toJSON b, toJSON c]

data Bar = Bar { bar1 :: String
               , bar2 :: Maybe String
               , bar3 :: String
               , bar4 :: Foo
               }

instance showBar :: Show Bar where
    show (Bar { bar1 = b1, bar2 = b2, bar3 = b3, bar4 = b4 }) =
        "Bar {bar1: " ++ show b1 ++
           ", bar2: " ++ show b2 ++
           ", bar3: " ++ show b3 ++
           ", bar4: " ++ show b4 ++ "}"

instance barFromJSON :: FromJSON Bar where
    parseJSON (JObject o) = do
        b1 <- o .:  "bar1"
        b2 <- o .:? "bar2"
        b3 <- o .:? "bar3" .!= "default"
        b4 <- o .:  "bar4"
        return $ Bar { bar1: b1, bar2: b2, bar3: b3, bar4: b4 }
    parseJSON _ = fail "Bar parse failed."

instance barToJSON :: ToJSON Bar where
    toJSON (Bar { bar1 = b1, bar2 = b2, bar3 = b3, bar4 = b4 }) =
        object ["bar1" .= b1, "bar2" .= b2, "bar3" .= b3, "bar4" .= b4]

data Baz = BazOne Number | BazMany (Array Baz)

instance showBaz :: Show Baz where
    show (BazOne num) = "BazOne " ++ show num
    show (BazMany bazs) = "BazMany " ++ show bazs

instance bazFromJSON :: FromJSON Baz where
    parseJSON (JObject o) = do
        t <- o .: "type"
        case t of
            "one" -> liftM1 BazOne (o .: "num")
            "many" -> liftM1 BazMany (o .: "bazs")
    parseJSON _ = fail "Baz parse fail"

instance bazToJSON :: ToJSON Baz where
    toJSON (BazOne num) = object ["type" .= "one", "num" .= num]
    toJSON (BazMany bazs) = object ["type" .= "many", "bazs" .= map toJSON bazs]


main = do
    Console.log "decode"
    Console.print (decode "{\"bar1\": \"bar1 value\", \"bar4\": [12,23,\"Kevin\"]}" :: Maybe Bar)
    Console.print (decode "{\"type\": \"many\", \"bazs\": [{\"type\": \"one\", \"num\": 123}]}" :: Maybe Baz)

    Console.log "\nencode"
    Console.log (encode $ Bar {bar1: "bar1 value", bar2: Nothing, bar3: "default", bar4: Foo 12.0 23.0 "Kevin"})
    Console.log (encode $ BazMany [BazOne 123.0])
