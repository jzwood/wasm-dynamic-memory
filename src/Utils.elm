module Utils exposing (..)

import List.Extra exposing (removeAt)


insert : a -> Int -> List a -> List a
insert x index list =
    List.take index list ++ x :: List.drop index list


move : ( a, Int ) -> Int -> List a -> List a
move ( x, c0 ) c1 xs =
    if c0 < c1 then
        removeAt c0 xs |> insert x (c1 - 1)

    else
        removeAt c0 xs |> insert x c1


unwords : List String -> String
unwords =
    String.join " "


strToInt : String -> Int
strToInt v =
    String.filter Char.isDigit v |> String.toInt |> Maybe.withDefault 0


ordered : Float -> Float -> Float -> Bool
ordered lo val hi =
    lo < val && val < hi
