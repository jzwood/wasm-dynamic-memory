module Utils exposing (..)


insert : a -> Int -> List a -> List a
insert x index list =
    List.take index list ++ x :: List.drop index list


remove : Int -> List a -> List a
remove index list =
    List.take index list ++ List.drop (index + 1) list


replace : a -> Int -> List a -> List a
replace x index list =
    List.take index list ++ x :: List.drop (index + 1) list
