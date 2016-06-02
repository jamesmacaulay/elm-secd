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
