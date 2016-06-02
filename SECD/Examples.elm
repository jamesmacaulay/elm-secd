module SECD.Examples exposing (..)

import SECD exposing (..)
import SECD.Expressions exposing (..)
import SECD.Arithmetic.Curried


helloWorld : Machine
helloWorld =
    initSimpleMachine
        <| ident "HelloWorld"


helloWorldIdentity : Machine
helloWorldIdentity =
    initSimpleMachine
        <| apply (fn "x" (ident "x")) (ident "HelloWorld")


churchTrue : Expression
churchTrue =
    (fn "a" (fn "b" (ident "a")))


churchFalse : Expression
churchFalse =
    (fn "a" (fn "b" (ident "b")))


showChurchTrue : Machine
showChurchTrue =
    initSimpleMachine
        <| apply2 churchTrue (ident "True") (ident "False")


showChurchFalse : Machine
showChurchFalse =
    initSimpleMachine
        <| apply2 churchFalse (ident "True") (ident "False")


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
