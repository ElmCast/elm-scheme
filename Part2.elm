module Main exposing (..)

import Html exposing (text)
import Combine exposing (..)
import Combine.Char as Combine exposing (..)
import Combine.Infix as Combine exposing (..)
import String


symbol : Parser Char
symbol =
    Combine.oneOf <| String.toList "!#$%&|*+-/:<=>?@^_~"


spaces : Parser ()
spaces =
    skipMany1 space


readExpr : String -> String
readExpr input =
    case parse (spaces *> symbol) input of
        ( Ok value, _ ) ->
            "Found value: " ++ (toString value)

        ( Err messages, _ ) ->
            "No match: " ++ (String.join "\n" messages)


main =
    text (readExpr " !")
