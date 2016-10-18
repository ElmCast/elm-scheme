module Main exposing (..)

import Html exposing (text)
import Combine exposing (..)
import Combine.Char as Combine exposing (..)
import String


symbol : Parser Char
symbol =
    Combine.oneOf <| String.toList "!#$%&|*+-/:<=>?@^_~"


readExpr : String -> String
readExpr input =
    case parse symbol input of
        ( Ok value, _ ) ->
            "Found value: " ++ (toString value)

        ( Err messages, _ ) ->
            "No match: " ++ (String.join "\n" messages)


main =
    text (readExpr "a")
