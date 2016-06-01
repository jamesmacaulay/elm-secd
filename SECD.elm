module SECD exposing (..)


constructMachine : Expression -> SECD
constructMachine expression =
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
                    Err ("could not find " ++ name ++ " in environment")

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
            Err (toString fstStack ++ " is not implemented as a primitive operator")

        ( stack, env, (AE (Combination rand rator)) :: tControl, dump ) ->
            Ok ( stack, env, AE rand :: AE rator :: Ap :: tControl, dump )

        _ ->
            Err "I don't know what to do"
