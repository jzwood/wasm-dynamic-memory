module Utils exposing (..)


insert : a -> Int -> List a -> List a
insert x index list =
    List.take index list ++ x :: List.drop index list
