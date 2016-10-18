module Main exposing (..)

import Html exposing (text)
import Combine exposing (..)
import Combine.Char as Combine exposing (..)
import Combine.Infix as Combine exposing (..)
import Combine.Num as Combine exposing (int)
import String


type LispVal
    = Atom String
    | List (List LispVal)
    | DottedList (List LispVal) LispVal
    | Number Int
    | String String
    | Bool Bool


spaces : Parser ()
spaces =
    skipMany1 space


symbol : Parser Char
symbol =
    Combine.oneOf <| String.toList "!#$%&|*+-/:<=>?@^_~"


parseString : Parser LispVal
parseString =
    String <$> (String.fromList <$> (char '"' *> many (noneOf [ '"' ]) <* char '"'))


parseBoolOrAtom : Parser LispVal
parseBoolOrAtom =
    let
        start =
            lower <|> upper <|> symbol

        rest =
            many (lower <|> upper <|> digit <|> symbol)

        atom string =
            case string of
                "#t" ->
                    Bool True

                "#f" ->
                    Bool False

                _ ->
                    Atom string
    in
        atom <$> (String.fromList <$> ((::) <$> start <*> rest))


parseNumber : Parser LispVal
parseNumber =
    Number <$> int


parseList : Parser LispVal
parseList =
    let
        list =
            List <$> (sepBy spaces parseExpr)
    in
        char '(' *> list <* char ')'


parseDottedList : Parser LispVal
parseDottedList =
    let
        head =
            sepEndBy spaces parseExpr

        tail =
            char '.' *> spaces *> parseExpr

        list =
            DottedList <$> head <*> tail
    in
        char '(' *> list <* char ')'


parseQuoted : Parser LispVal
parseQuoted =
    rec <|
        \() ->
            let
                quoted e =
                    [ Atom "quote", e ]
            in
                List <$> (quoted <$> (char '\'' *> parseExpr))


parseExpr : Parser LispVal
parseExpr =
    rec <|
        \() ->
            choice
                [ parseBoolOrAtom
                , parseString
                , parseNumber
                , parseQuoted
                , parseList
                , parseDottedList
                ]


readExpr : String -> String
readExpr input =
    case parse parseExpr input of
        ( Ok value, _ ) ->
            "Found value: " ++ (toString value)

        ( Err messages, _ ) ->
            "No match: " ++ (String.join "\n" messages)


main =
    text (readExpr "(a \"Hello\" . ('c #f 4))")
