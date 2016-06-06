module SECD.Printer exposing (..)

import SECD exposing (..)
import String


instructionToString : Instruction -> String
instructionToString instruction =
    case instruction of
        Ap ->
            "apply"

        AE expression ->
            "expression: " ++ (expressionToString expression)


expressionToString : Expression -> String
expressionToString expression =
    case expression of
        Identifier name ->
            name

        Lambda bv body ->
            let
                bvString =
                    case bv of
                        SingleVariable var ->
                            var

                        VariableList vars ->
                            "(" ++ String.join "," vars ++ ")"
            in
                "λ" ++ bvString ++ "." ++ expressionToString body

        Combination operator operand ->
            let
                operatorString =
                    case operator of
                        Lambda _ _ ->
                            "(" ++ expressionToString operator ++ ")"

                        _ ->
                            expressionToString operator

                operandString =
                    case operand of
                        Identifier _ ->
                            expressionToString operand

                        _ ->
                            "(" ++ expressionToString operand ++ ")"
            in
                operatorString ++ " " ++ operandString
