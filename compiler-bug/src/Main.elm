module Main exposing (..)

import Html exposing (text)
import List.Extra exposing (mapAccuml)


main =
    text "Hello!"


type Instr
    = EmptyLine
    | Block String Int
    | End


indentLines : List Instr -> List ( Int, Instr )
indentLines ast =
    List.Extra.mapAccuml
        (\indent instr ->
            let
                indentedLine : ( Int, Instr )
                indentedLine =
                    ( indent + 1, instr )

                neutralLine : ( Int, Instr )
                neutralLine =
                    ( indent, instr )

                unindentedLine : ( Int, instr )
                unindentedLine =
                    ( indent - 1, instr )
            in
            case instr of
                Block _ _ ->
                    indentedLine

                End ->
                    unindentedLine

                op ->
                    neutralLine
        )
        ( 0, 0 )
        ast
