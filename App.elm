module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (onClick)



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


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 2

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ section [ id "code" ] [ text "code" ]
        , section [ id "opcodes" ]
            [ div [ draggable "true" ] [ text "func" ]
            , div [ draggable "true" ] [ text "var" ]
            ]
        ]
