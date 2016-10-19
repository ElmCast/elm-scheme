module Expression exposing (..)

import Combine
import Combine.Char as Combine
import Combine.Num as Combine
import Combine.Infix exposing (..)
import String


type Expression
    = ENumber Float
    | EBool Bool
    | EString String
    | EIdentifier String
    | EList (List Expression)


numberParser : Combine.Parser Expression
numberParser =
    ENumber <$> Combine.choice [ Combine.float, toFloat <$> Combine.int ]


readExpression : String -> String
readExpression input =
    case Combine.parse numberParser input of
        ( Ok value, _ ) ->
            "Found value: " ++ (toString value)

        ( Err messages, _ ) ->
            "No match: " ++ (String.join "\n" messages)
