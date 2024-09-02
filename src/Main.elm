module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (on, onClick)
import Instructions exposing (..)
import Json.Decode as Decode



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Module
    = Module Children


type alias Model =
    { ast : List Instruction, cursor : Maybe Int }


type Msg
    = Increment
    | Decrement


init : Model
init =
    { ast = List.repeat 50 EmptyLine, cursor = Nothing }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model


onDragEnter : msg -> Attribute msg
onDragEnter message =
    on "click" (Decode.succeed message)



-- VIEW


view : Model -> Html Msg
view { ast, cursor } =
    div []
        [ section [ id "code" ]
            (List.indexedMap
                (\i x ->
                    div
                        [ class "line"
                        , class
                            (if Just i == cursor then
                                "cursor"

                             else
                                ""
                            )
                        ]
                        [ text "" ]
                )
                ast
            )
        , section [ id "messages" ] [ text "messages" ]
        , section [ id "instructions" ]
            (toHtml instructions)
        ]


toHtml : List Instr -> List (Html Msg)
toHtml ins =
    List.map (\(Instr ui) -> div [ class "instr", draggable "true" ] [ text ui.button ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
