module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Debug.Trace

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple
import Data.Maybe
import Data.Either

import Data.JSON

test :: forall a r. (Eq a, Show a) => String -> a -> a
     -> Eff (err :: Exception String, trace :: Trace | r) Unit
test t a b =
    if a /= b
    then throwException $ t ++ " fail. expected: " ++ show a ++ ", but actual: " ++ show b
    else Debug.Trace.trace $ t ++ " success."

main = do
    test "Number" (Just 12)    (decode "12" :: Maybe Number)
    test "String" (Just "foo") (decode "\"foo\"" :: Maybe String)
    test "Bool"   (Just true)  (decode "true" :: Maybe Boolean)
    test "Unit"   (Just unit)  (decode "null" :: Maybe Unit)

    test "Array"  (Just [1,2,3,2,1]) (decode "[1,2,3,2,1]" :: Maybe [Number])
    test "Set"    (Just (S.fromList [1,2,3])) (decode "[1,2,3,2,1]" :: Maybe (S.Set Number))
    test "Tuple"  (Just (Tuple "kevin" 18)) (decode "[\"kevin\", 18]" :: Maybe (Tuple String Number))
    test "Map"    (Just (M.fromList [Tuple "a" 3, Tuple "b" 2])) (decode "{\"a\": 1, \"b\": 2, \"a\": 3}" :: Maybe (M.Map String Number))

    test "Nothing" (Just Nothing)  (decode "\"a\"" :: Maybe (Maybe Number))
    test "Just"    (Just (Just 3)) (decode "3" :: Maybe (Maybe Number))

    test "Left"    (Just (Left 4))     (decode "{\"Left\": 4}" :: Maybe (Either Number Boolean))
    test "Right"   (Just (Right true)) (decode "{\"Right\": true}" :: Maybe (Either Number Boolean))
    test "Both"    Nothing             (decode "{\"Left\": 4, \"Right\": true}" :: Maybe (Either Number Boolean))

    test "Value"   (Just (Array [Number 1, Bool true, Object $ M.fromList [Tuple "foo" (Number 12), Tuple "bar" (Array [String "baz", Number 43])]]))
        (decode "[1,true,{\"foo\": 12, \"bar\": [\"baz\", 43]}]" :: Maybe Value)
