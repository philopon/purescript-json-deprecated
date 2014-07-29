module Main where

import Control.Monad.Eff

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
    parseJSON (Array [a,b,c]) =
        Foo <$> parseJSON a <*> parseJSON b <*> parseJSON c
    parseJSON _ = fail "Foo parse failed."

instance fooToJSON :: ToJSON Foo where
    toJSON (Foo a b c) = Array [toJSON a, toJSON b, toJSON c]

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
    parseJSON (Object o) = do
        b1 <- o .:  "bar1"
        b2 <- o .:? "bar2"
        b3 <- o .:? "bar3" .!= "default"
        b4 <- o .:  "bar4"
        return $ Bar { bar1: b1, bar2: b2, bar3: b3, bar4: b4 }
    parseJSON _ = fail "Bar parse failed."

instance barToJSON :: ToJSON Bar where
    toJSON (Bar { bar1 = b1, bar2 = b2, bar3 = b3, bar4 = b4 }) =
        object ["bar1" .= b1, "bar2" .= b2, "bar3" .= b3, "bar4" .= b4]

main :: Eff (trace :: Debug.Trace.Trace) Unit
main = do
    Debug.Trace.trace "decode"
    Debug.Trace.print (decode "{\"bar1\": \"bar1 value\", \"bar4\": [12,23,\"Kevin\"]}" :: Maybe Bar)

    Debug.Trace.trace "\nencode"
    Debug.Trace.trace (encode $ Bar {bar1: "bar1 value", bar2: Nothing, bar3: "default", bar4: Foo 12 23 "Kevin"})

