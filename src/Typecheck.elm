module Typecheck exposing (..)

import Dict exposing (Dict)
import Instructions exposing (..)
import List.Extra exposing (find, last, scanl)
import Result exposing (Result)


type Error
    = UnbalancedParams


type alias Typecheck =
    Result { line : Cursor, error : Error } ()


typecheck : List Instr -> Typecheck
typecheck instrs =
    balancedParens instrs


balancedParens : List Instr -> Typecheck
balancedParens instrs =
    let
        getIndent instr =
            case instr of
                Fun _ ->
                    1

                Loop _ ->
                    1

                If _ ->
                    1

                Block _ ->
                    1

                End ->
                    -1

                op ->
                    0

        alg instr ( line, indent ) =
            ( line + 1, indent + getIndent instr )

        indentations =
            scanl alg ( 0, 0 ) instrs

        ( lastLine, lastIndentation ) =
            List.foldl alg ( 0, 0 ) instrs

        subZero =
            find (\( _, indent ) -> indent < 0) indentations
    in
    case subZero of
        Nothing ->
            if lastIndentation > 0 then
                Err { line = lastLine, error = UnbalancedParams }

            else
                Ok ()

        Just ( line, indent ) ->
            Err { line = line, error = UnbalancedParams }


topLevelFunctions : List Instr -> Typecheck
topLevelFunctions _ =
    Ok ()


arityCheck : List Instr -> Typecheck
arityCheck _ =
    Ok ()


arityLookup : List Instr -> Dict Instr ( Arity, Coarity )
arityLookup _ =
    Dict.empty
