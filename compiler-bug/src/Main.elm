module Main exposing (..)

import Html exposing (text)
import List.Extra exposing (mapAccuml)


main =
    text "Hello!"


func =
    List.Extra.mapAccuml
        (\a b ->
            let
                return : ( Int, x )
                return =
                    ( a, b )
            in
            return
        )
        ( 0, 0 )
