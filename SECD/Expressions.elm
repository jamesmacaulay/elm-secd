module SECD.Expressions exposing (..)

import SECD exposing (..)


fn : String -> Expression -> Expression
fn var body =
    Lambda (SingleVariable var) body


ident : String -> Expression
ident =
    Identifier


apply : Expression -> Expression -> Expression
apply =
    Combination


apply2 : Expression -> Expression -> Expression -> Expression
apply2 operator operand1 operand2 =
    apply (apply operator operand1) operand2


identityLambda : Expression
identityLambda =
    fn "x" (ident "x")


churchEncode : Int -> Expression
churchEncode n =
    let
        body n' =
            if n' == 0 then
                (ident "x")
            else
                apply (ident "f") (body (n - 1))
    in
        fn "f" (fn "x" (body n))


churchPlus : Expression
churchPlus =
    fn "m"
        (fn "n"
            (fn "f"
                (fn "x"
                    (apply2 (ident "m")
                        (ident "f")
                        (apply2 (ident "n") (ident "f") (ident "x"))
                    )
                )
            )
        )


churchSucc : Expression
churchSucc =
    fn "n"
        (fn "f"
            (fn "x"
                (apply (ident "f")
                    (apply2 (ident "n") (ident "f") (ident "x"))
                )
            )
        )


churchTrue : Expression
churchTrue =
    fn "a" (fn "b" (ident "a"))


churchFalse : Expression
churchFalse =
    fn "a" (fn "b" (ident "b"))


churchIsZero : Expression
churchIsZero =
    fn "n" (apply2 (ident "n") (fn "x" churchFalse) churchTrue)


identityOfIdentity : Expression
identityOfIdentity =
    apply identityLambda identityLambda


succZero : Expression
succZero =
    apply churchSucc (churchEncode 0)


onePlusTwo : Expression
onePlusTwo =
    apply2 (ident "+") (ident "1") (ident "2")


plusAndMinusExample : Expression
plusAndMinusExample =
    apply2 (ident "+")
        (apply2 (ident "-")
            (ident "11")
            (ident "2")
        )
        (ident "5")


isZeroZero : Expression
isZeroZero =
    apply churchIsZero (churchEncode 0)


isSuccZeroZero : Expression
isSuccZeroZero =
    apply2 (apply churchIsZero succZero) churchTrue churchFalse
