module Main exposing (..)

import Html exposing (..)
import Expression exposing (..)


main =
    text (readExpression "123.45")
