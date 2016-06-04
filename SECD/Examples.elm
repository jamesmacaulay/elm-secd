module SECD.Examples exposing (..)

import SECD exposing (..)
import SECD.ExpressionHelpers exposing (..)
import SECD.CurriedArithmetic
import SECD.ChurchEncoding as CE


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
        <| CE.showBoolean CE.chTrue


showChurchFalse : Machine
showChurchFalse =
    initSimpleMachine
        <| CE.showBoolean CE.chFalse


zeroIsZero : Machine
zeroIsZero =
    initSimpleMachine
        <| (CE.showBoolean (apply CE.isZero (CE.encodeInt 0)))


oneMinusOneIsZero : Machine
oneMinusOneIsZero =
    initSimpleMachine
        <| (CE.showBoolean (apply CE.isZero (apply2 CE.minus (CE.encodeInt 1) (CE.encodeInt 1))))


twoMinusOneIsNotZero : Machine
twoMinusOneIsNotZero =
    initSimpleMachine
        <| (CE.showBoolean (apply CE.isZero (apply2 CE.minus (CE.encodeInt 2) (CE.encodeInt 1))))


onePlusTwo : Machine
onePlusTwo =
    initMachine SECD.CurriedArithmetic.basicFunctions
        <| apply2 (ident "+") (ident "1") (ident "2")


plusAndMinus : Machine
plusAndMinus =
    initMachine SECD.CurriedArithmetic.basicFunctions
        <| apply2 (ident "+")
            (apply2 (ident "-")
                (ident "11")
                (ident "2")
            )
            (ident "5")
