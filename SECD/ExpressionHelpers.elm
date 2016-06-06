module SECD.ExpressionHelpers exposing (..)

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


applyArg : Expression -> Expression -> Expression
applyArg =
    flip apply


apply2 : Expression -> Expression -> Expression -> Expression
apply2 operator operand1 operand2 =
    apply (apply operator operand1) operand2


apply3 : Expression -> Expression -> Expression -> Expression -> Expression
apply3 operator operand1 operand2 operand3 =
    apply (apply (apply operator operand1) operand2) operand3
