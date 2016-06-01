module SECD exposing (..)

import Dict exposing (Dict)
import String


empty : SECD
empty =
    ( [], [], [], NoDump )


construct : Expression -> SECD
construct expression =
    ( [], [], [ AE expression ], NoDump )


type alias SECD =
    ( Stack, Environment, Control, Dump )


type alias Stack =
    List Value


type alias Environment =
    List ( String, Value )


type alias Control =
    List Instruction


type Dump
    = Dump SECD
    | NoDump


type BoundVariables
    = SingleVariable String
    | VariableList (List String)


type Expression
    = Identifier String
    | Lambda BoundVariables Expression
    | Combination Expression Expression


type Value
    = Atom String
    | Closure ( Environment, BoundVariables ) Expression
    | BasicFunction (Value -> Result String Value)
    | ValueList (List Value)


type Instruction
    = Ap
    | AE Expression


location : Environment -> String -> Maybe Value
location environment name =
    case environment of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if name == k then
                Just v
            else
                location rest name


assoc : BoundVariables -> Value -> Result String Environment
assoc boundVars value =
    case boundVars of
        VariableList vars ->
            case value of
                ValueList values ->
                    Ok (List.map2 (,) vars values |> List.reverse)

                _ ->
                    Err "could not decompose non-list value into variable list"

        SingleVariable var ->
            Ok [ ( var, value ) ]


derive : Environment -> Environment -> Environment
derive new base =
    new ++ base


valueToInt : Value -> Result String Int
valueToInt val =
    case val of
        Atom intStr ->
            String.toInt intStr

        Closure _ _ ->
            Err "could not convert closure to integer"

        BasicFunction _ ->
            Err "could not convert basic function to integer"

        ValueList _ ->
            Err "could not convert list to integer"


basicUnaryIntegerArithmetic : (Int -> Int -> Int) -> Int -> (Value -> Result String Value)
basicUnaryIntegerArithmetic f x =
    valueToInt >> Result.map (f x >> toString >> Atom)


basicBinaryIntegerArithmetic : (Int -> Int -> Int) -> (Value -> Result String Value)
basicBinaryIntegerArithmetic f =
    valueToInt >> Result.map (basicUnaryIntegerArithmetic f >> BasicFunction)


basicPlusX : Int -> (Value -> Result String Value)
basicPlusX =
    basicUnaryIntegerArithmetic (+)


basicPlus : Value -> Result String Value
basicPlus =
    basicBinaryIntegerArithmetic (+)


basicMinusX : Int -> (Value -> Result String Value)
basicMinusX =
    basicUnaryIntegerArithmetic (-)


basicMinus : Value -> Result String Value
basicMinus =
    basicBinaryIntegerArithmetic (-)


basicFunctions : Dict String (Value -> Result String Value)
basicFunctions =
    Dict.fromList
        [ ( "+", basicPlus )
        , ( "-", basicMinus )
        ]


applyPrimitiveToOperand : Value -> Value -> Result String Value
applyPrimitiveToOperand operand operator =
    case operator of
        BasicFunction f ->
            f operand

        x ->
            Err ("expected a basic function but got " ++ toString x)


transform : SECD -> Result String SECD
transform secd =
    case secd of
        ( hS :: _, _, [], Dump ( s', e', c', d' ) ) ->
            Ok ( hS :: s', e', c', d' )

        ( [], _, [], _ ) ->
            Err "empty control with empty stack"

        ( _, _, [], NoDump ) ->
            Err "done!"

        ( stack, env, (AE (Identifier name)) :: tControl, dump ) ->
            case location env name of
                Nothing ->
                    case Dict.get name basicFunctions of
                        Nothing ->
                            Ok ( Atom name :: stack, env, tControl, dump )

                        Just f ->
                            Ok ( BasicFunction f :: stack, env, tControl, dump )

                Just value ->
                    Ok ( value :: stack, env, tControl, dump )

        ( stack, env, (AE (Lambda boundVars body)) :: tControl, dump ) ->
            Ok ( Closure ( env, boundVars ) body :: stack, env, tControl, dump )

        ( (Closure ( closureBaseEnv, boundVars ) expression) :: sndStack :: ttStack, env, Ap :: tControl, dump ) ->
            assoc boundVars sndStack
                `Result.andThen` (\boundVarsEnv ->
                                    let
                                        newEnv =
                                            derive boundVarsEnv closureBaseEnv

                                        dump' =
                                            Dump ( ttStack, env, tControl, dump )
                                    in
                                        Ok ( [], newEnv, [ AE expression ], dump' )
                                 )

        ( fstStack :: sndStack :: ttStack, env, Ap :: tControl, dump ) ->
            fstStack
                |> applyPrimitiveToOperand sndStack
                |> Result.map (\value -> ( value :: ttStack, env, tControl, dump ))

        ( stack, env, (AE (Combination rand rator)) :: tControl, dump ) ->
            Ok ( stack, env, AE rator :: AE rand :: Ap :: tControl, dump )

        _ ->
            Err "I don't know what to do"
