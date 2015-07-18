module Examples.Simple where

import Prelude

import Control.Monad.Eff
import qualified Control.Monad.Eff.Console as Console

import Data.List(List(..), (:))
import Data.Maybe
import Data.Either
import Data.JSON
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    Console.log "decode"
    Console.print (decode "12"      :: Maybe Number)
    Console.print (decode "\"foo\"" :: Maybe String)
    Console.print (decode "true"    :: Maybe Boolean)
    Console.print (decode "null"    :: Maybe Unit)

    Console.print (decode "[1,\"Number\"]" :: Maybe (Tuple Number String))
    Console.print (decode "[1,2,3,2]" :: Maybe (Array Number))
    Console.print (decode "[1,2,3,2]" :: Maybe (S.Set Number))
    Console.print (decode "{\"foo\": 1, \"bar\": 2}" :: Maybe (M.Map String Number))

    Console.print (decode "12" :: Maybe (Maybe String))
    Console.print (decode "{\"Left\": true}" :: Maybe (Either Boolean Number))


    Console.print (decode "[{\"foo\": [1,2,3]}, true]" :: Maybe JValue)

    Console.log "\nencode"
    Console.log (encode 12.0)
    Console.log (encode "foo")
    Console.log (encode true)
    Console.log (encode unit)

    Console.log (encode $ Tuple 1.0 "Number")
    Console.log (encode $ [1.0,2.0,3.0,2.0])
    Console.log (encode $ S.fromList $ 1.0:2.0:3.0:2.0:Nil)
    Console.log (encode $ M.fromList $ Tuple "foo" 1.0 : Tuple "bar" 2.0 : Nil)

    Console.log (encode (Just 12.0))
    Console.log (encode (Left true :: Either Boolean Number))

    Console.log (encode $ JArray [object ["foo" .= JArray [JNumber 1.0,JNumber 2.0,JNumber 3.0]], JBool true])
