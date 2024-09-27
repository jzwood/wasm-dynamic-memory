module Typecheck exposing (..)

import Dict exposing (Dict)
import Instructions exposing (..)
import Result exposing (Result)


type alias Typecheck =
    Result { line : Cursor, error : String } (List Instr)


balancedParens : List Instr -> Typecheck
balancedParens _ =
    Ok []


topLevelFunctions : List Instr -> Typecheck
topLevelFunctions _ =
    Ok []


arityCheck : List Instr -> Typecheck
arityCheck _ =
    Ok []


arityLookup : List Instr -> Dict Instr ( Arity, Coarity )
arityLookup _ =
    Dict.empty
