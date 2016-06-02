module SECD exposing (..)

import Dict exposing (Dict)
import String


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


type Value
    = Atom String
    | Closure ( Environment, BoundVariables ) Expression
    | BasicFunction (Value -> Result String Value)
    | ListValue (List Value)


type BoundVariables
    = SingleVariable String
    | VariableList (List String)


type Expression
    = Identifier String
    | Lambda BoundVariables Expression
    | Combination Expression Expression


type Instruction
    = Ap
    | AE Expression


type alias Machine =
    { basicFunctions : BasicFunctionDict
    , state : SECD
    }


type alias BasicFunctionDict =
    Dict String (Value -> Result String Value)


emptyMachine : BasicFunctionDict -> Machine
emptyMachine basicFunctions =
    Machine basicFunctions ( [], [], [], NoDump )


pushInstruction : Instruction -> Machine -> Machine
pushInstruction instruction ({ state } as machine) =
    let
        ( s, e, c, d ) =
            state
    in
        { machine | state = ( s, e, instruction :: c, d ) }


initMachine : BasicFunctionDict -> Expression -> Machine
initMachine basicFunctions expression =
    emptyMachine basicFunctions |> pushInstruction (AE expression)


initSimpleMachine : Expression -> Machine
initSimpleMachine expression =
    emptyMachine Dict.empty |> pushInstruction (AE expression)


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
                ListValue values ->
                    Ok (List.map2 (,) vars values |> List.reverse)

                _ ->
                    Err "could not decompose non-list value into variable list"

        SingleVariable var ->
            Ok [ ( var, value ) ]


derive : Environment -> Environment -> Environment
derive new base =
    new ++ base


transform : Machine -> Result String Machine
transform ({ state, basicFunctions } as machine) =
    case state of
        ( hS :: _, _, [], Dump ( s', e', c', d' ) ) ->
            Ok { machine | state = ( hS :: s', e', c', d' ) }

        ( [], _, [], _ ) ->
            Err "invalid state: empty control with empty stack"

        ( _, _, [], NoDump ) ->
            Err "already finished!"

        ( stack, env, (AE (Identifier name)) :: tControl, dump ) ->
            case location env name of
                Nothing ->
                    case Dict.get name basicFunctions of
                        Nothing ->
                            Ok { machine | state = ( Atom name :: stack, env, tControl, dump ) }

                        Just f ->
                            Ok { machine | state = ( BasicFunction f :: stack, env, tControl, dump ) }

                Just value ->
                    Ok { machine | state = ( value :: stack, env, tControl, dump ) }

        ( stack, env, (AE (Lambda boundVars body)) :: tControl, dump ) ->
            Ok { machine | state = ( Closure ( env, boundVars ) body :: stack, env, tControl, dump ) }

        ( (Closure ( closedOverEnv, boundVars ) expression) :: sndStack :: ttStack, env, Ap :: tControl, dump ) ->
            assoc boundVars sndStack
                `Result.andThen` (\boundVarsEnv ->
                                    let
                                        newEnv =
                                            derive boundVarsEnv closedOverEnv

                                        dump' =
                                            Dump ( ttStack, env, tControl, dump )
                                    in
                                        Ok { machine | state = ( [], newEnv, [ AE expression ], dump' ) }
                                 )

        ( (BasicFunction f) :: sndStack :: ttStack, env, Ap :: tControl, dump ) ->
            f sndStack
                |> Result.map (\value -> { machine | state = ( value :: ttStack, env, tControl, dump ) })

        ( stack, env, (AE (Combination rand rator)) :: tControl, dump ) ->
            Ok { machine | state = ( stack, env, AE rator :: AE rand :: Ap :: tControl, dump ) }

        _ ->
            Err "I don't know what to do"
