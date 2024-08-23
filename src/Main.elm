module Main exposing (..)

import Browser
import Html exposing (Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (onClick)
import Operators exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ section [ id "code" ] [ text "code" ]
        , section [ id "messages" ] [ text "messages" ]
        , section [ id "opcodes" ]
            (List.map showOperator operators)
        ]
