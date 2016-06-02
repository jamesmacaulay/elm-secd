module SECD.Examples exposing (..)

import SECD exposing (..)
import SECD.Expressions exposing (..)
import SECD.Arithmetic.Curried


helloWorld : Machine
helloWorld =
    initSimpleMachine
        <| ident "hello"


helloWorldIdentity : Machine
helloWorldIdentity =
    initSimpleMachine
        <| apply (fn "x" (ident "x")) (ident "hello world")


onePlusTwo : Machine
onePlusTwo =
    initMachine SECD.Arithmetic.Curried.basicFunctions
        <| apply2 (ident "+") (ident "1") (ident "2")


plusAndMinus : Machine
plusAndMinus =
    initMachine SECD.Arithmetic.Curried.basicFunctions
        <| apply2 (ident "+")
            (apply2 (ident "-")
                (ident "11")
                (ident "2")
            )
            (ident "5")
