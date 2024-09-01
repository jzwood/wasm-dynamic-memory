module Main exposing (..)

import Browser
import Html exposing (Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (onClick)
import Instructions exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Module
    = Module Children


type alias Model =
    { ast : List Instruction }


init : Model
init =
    { ast = List.repeat 50 EmptyLine }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view { ast } =
    div []
        [ section [ id "code" ]
            (List.map (\x -> div [ class "line" ] [ text "" ]) ast)
        , section [ id "messages" ] [ text "messages" ]
        , section [ id "instructions" ]
            (toHtml instructions)
        ]
