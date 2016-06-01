module SECD exposing (..)


identityLambda : Expression
identityLambda =
    Lambda (SingleVariable "x")
        (Identifier "x")


churchEncode : Int -> Expression
churchEncode n =
    let
        body n' =
            if n' == 0 then
                (Identifier "x")
            else
                Combination (Identifier "f") (body (n - 1))
    in
        Lambda (SingleVariable "f")
            (Lambda (SingleVariable "x")
                (body n)
            )


churchPlus : Expression
churchPlus =
    Lambda (SingleVariable "m")
        (Lambda (SingleVariable "n")
            (Lambda (SingleVariable "f")
                (Lambda (SingleVariable "x")
                    (Combination (Combination (Identifier "m") (Identifier "f"))
                        (Combination (Combination (Identifier "n") (Identifier "f"))
                            (Identifier "x")
                        )
                    )
                )
            )
        )


identityOfIdentityMachine : SECD
identityOfIdentityMachine =
    constructMachine (Combination identityLambda identityLambda)


onePlusOneMachine : SECD
onePlusOneMachine =
    constructMachine (Combination (Combination churchPlus (churchEncode 1)) (churchEncode 1))


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
                |> Debug.log "( hS :: _, _, [], Dump ( s', e', c', d' ) )"

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
                        |> Debug.log "( stack, env, (AE (Identifier name)) :: tControl, dump )"

        ( stack, env, (AE (Lambda boundVars body)) :: tControl, dump ) ->
            Ok ( Closure ( env, boundVars ) body :: stack, env, tControl, dump )
                |> Debug.log "( stack, env, (AE (Lambda boundVars body)) :: tControl, dump )"

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
                                            |> Debug.log "( (Closure ( closureBaseEnv, boundVars ) expression) :: sndStack :: ttStack, env, Ap :: tControl, dump )"
                                 )

        ( fstStack :: sndStack :: ttStack, env, Ap :: tControl, dump ) ->
            Err (toString fstStack ++ " is not implemented as a primitive operator")

        ( stack, env, (AE (Combination rand rator)) :: tControl, dump ) ->
            Ok ( stack, env, AE rand :: AE rator :: Ap :: tControl, dump )
                |> Debug.log "( stack, env, (AE (Combination rand rator)) :: tControl, dump )"

        _ ->
            Err "I don't know what to do"
