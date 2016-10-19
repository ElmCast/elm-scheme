port module Main exposing (..)

import Expressions
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit Expressions.all


port emit : ( String, Value ) -> Cmd msg
