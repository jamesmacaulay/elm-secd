module SECD.Arithmetic.Curried exposing (..)

import SECD exposing (..)
import String
import Dict exposing (Dict)


valueToInt : Value -> Result String Int
valueToInt val =
    case val of
        Atom intStr ->
            String.toInt intStr

        Closure _ _ ->
            Err "could not convert closure to integer"

        BasicFunction _ ->
            Err "could not convert basic function to integer"

        ListValue _ ->
            Err "could not convert list to integer"


basicBinaryIntegerArithmetic : (Int -> Int -> Int) -> (Value -> Result String Value)
basicBinaryIntegerArithmetic f =
    let
        partialF x =
            valueToInt >> Result.map (f x >> toString >> Atom)
    in
        valueToInt >> Result.map (partialF >> BasicFunction)


plus : Value -> Result String Value
plus =
    basicBinaryIntegerArithmetic (+)


minus : Value -> Result String Value
minus =
    basicBinaryIntegerArithmetic (-)


basicFunctions : Dict String (Value -> Result String Value)
basicFunctions =
    Dict.fromList
        [ ( "+", plus )
        , ( "-", minus )
        ]
