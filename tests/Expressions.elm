module Expressions exposing (..)

import Combine
import Expect
import Expression exposing (..)
import String
import Test exposing (..)


testParser : Combine.Parser a -> String -> a -> Expect.Expectation
testParser parser input expectedValue =
    case Combine.parse parser input of
        ( Ok value, _ ) ->
            Expect.equal value expectedValue

        ( Err messages, _ ) ->
            Expect.fail (String.join "\n" messages)


all : Test
all =
    describe "number parser"
        [ test "positive float" <|
            \() ->
                testParser numberParser "1.1" (ENumber 1.1)
        , test "negative float" <|
            \() ->
                testParser numberParser "-1.1" (ENumber -1.1)
        , test "positive integer" <|
            \() ->
                testParser numberParser "1" (ENumber 1)
        , test "negative integer" <|
            \() ->
                testParser numberParser "-1" (ENumber -1)
        ]
