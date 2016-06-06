module SECD.ChurchEncoding exposing (..)

import SECD exposing (..)
import SECD.ExpressionHelpers exposing (..)


chTrue : Expression
chTrue =
    fn "a" (fn "b" (ident "a"))


chFalse : Expression
chFalse =
    fn "a" (fn "b" (ident "b"))


chAnd : Expression
chAnd =
    fn "p"
        (fn "q"
            (apply2 (ident "p") (ident "q") (ident "p"))
        )


chOr : Expression
chOr =
    fn "p"
        (fn "q"
            (apply2 (ident "p") (ident "p") (ident "q"))
        )


chNot : Expression
chNot =
    fn "p"
        (fn "a"
            (fn "b"
                (apply2 (ident "p") (ident "b") (ident "a"))
            )
        )


chXor : Expression
chXor =
    fn "a"
        (fn "b"
            (apply2 (ident "a") (apply chNot (ident "b")) (ident "b"))
        )


chIf : Expression
chIf =
    fn "p"
        (fn "a"
            (fn "b"
                (apply2 (ident "p") (ident "a") (ident "b"))
            )
        )


encodeInt : Int -> Expression
encodeInt n =
    let
        body n' =
            if n' <= 0 then
                (ident "x")
            else
                apply (ident "f") (body (n' - 1))
    in
        fn "f" (fn "x" (body n))


succ : Expression
succ =
    fn "n"
        (fn "f"
            (fn "x"
                (apply (ident "f")
                    (apply2 (ident "n") (ident "f") (ident "x"))
                )
            )
        )


plus : Expression
plus =
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


multiply : Expression
multiply =
    fn "m"
        (fn "n"
            (fn "f"
                (apply (ident "m") (apply (ident "n") (ident "f")))
            )
        )


exp : Expression
exp =
    fn "m"
        (fn "n"
            (apply (ident "n") (ident "m"))
        )


pred : Expression
pred =
    fn "n"
        (fn "f"
            (fn "x"
                (apply3 (ident "n")
                    (fn "g"
                        (fn "h"
                            (apply (ident "h") (apply (ident "g") (ident "f")))
                        )
                    )
                    (fn "u" (ident "x"))
                    (fn "u" (ident "u"))
                )
            )
        )


minus : Expression
minus =
    fn "m"
        (fn "n"
            (apply (apply (ident "n") pred) (ident "m"))
        )


isZero : Expression
isZero =
    fn "n" (apply2 (ident "n") (fn "x" chFalse) chTrue)


showBoolean : Expression -> Expression
showBoolean expression =
    (apply2 expression (ident "True") (ident "False"))
