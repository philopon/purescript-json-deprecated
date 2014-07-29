module Simple where

import Control.Monad.Eff

import Data.Maybe
import Data.Either
import Data.JSON
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

main :: Eff (trace :: Debug.Trace.Trace) Unit
main = do
    Debug.Trace.print (decode "12"      :: Maybe Number)
    Debug.Trace.print (decode "\"foo\"" :: Maybe String)
    Debug.Trace.print (decode "true"    :: Maybe Boolean)
    Debug.Trace.print (decode "null"    :: Maybe Unit)

    Debug.Trace.print (decode "[1,\"Number\"]" :: Maybe (Tuple Number String))
    Debug.Trace.print (decode "[1,2,3,2]" :: Maybe [Number])
    Debug.Trace.print (decode "[1,2,3,2]" :: Maybe (S.Set Number))
    Debug.Trace.print (eitherDecode "{\"foo\": 1, \"bar\": 2}" :: Either String (M.Map String Number))

    Debug.Trace.print (decode "12" :: Maybe (Maybe String))
    Debug.Trace.print (decode "{\"Left\": true}" :: Maybe (Either Boolean Number))


    Debug.Trace.print (decode "[{\"foo\": [1,2,3]}, true]" :: Maybe Value)
