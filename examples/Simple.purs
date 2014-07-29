module Main where

import Control.Monad.Eff

import Data.Maybe
import Data.Either
import Data.JSON
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

main :: Eff (trace :: Debug.Trace.Trace) Unit
main = do
    Debug.Trace.trace "decode"
    Debug.Trace.print (decode "12"      :: Maybe Number)
    Debug.Trace.print (decode "\"foo\"" :: Maybe String)
    Debug.Trace.print (decode "true"    :: Maybe Boolean)
    Debug.Trace.print (decode "null"    :: Maybe Unit)

    Debug.Trace.print (decode "[1,\"Number\"]" :: Maybe (Tuple Number String))
    Debug.Trace.print (decode "[1,2,3,2]" :: Maybe [Number])
    Debug.Trace.print (decode "[1,2,3,2]" :: Maybe (S.Set Number))
    Debug.Trace.print (decode "{\"foo\": 1, \"bar\": 2}" :: Maybe (M.Map String Number))

    Debug.Trace.print (decode "12" :: Maybe (Maybe String))
    Debug.Trace.print (decode "{\"Left\": true}" :: Maybe (Either Boolean Number))


    Debug.Trace.print (decode "[{\"foo\": [1,2,3]}, true]" :: Maybe Value)

    Debug.Trace.trace "\nencode"
    Debug.Trace.trace (encode 12)
    Debug.Trace.trace (encode "foo")
    Debug.Trace.trace (encode true)
    Debug.Trace.trace (encode unit)

    Debug.Trace.trace (encode $ Tuple 1 "Number")
    Debug.Trace.trace (encode $ [1,2,3,2])
    Debug.Trace.trace (encode $ S.fromList [1,2,3,2])
    Debug.Trace.trace (encode $ M.fromList [Tuple "foo" 1, Tuple "bar" 2])

    Debug.Trace.trace (encode (Just 12))
    Debug.Trace.trace (encode (Left true :: Either Boolean Number))

    Debug.Trace.trace (encode $ Array [object ["foo" .= Array [Number 1,Number 2,Number 3]], Bool true])

