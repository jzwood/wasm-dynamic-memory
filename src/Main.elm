module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (on, onClick, preventDefaultOn)
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
    = SetCursor (Maybe Int)
    | Nop


init : Model
init =
    { ast = List.repeat 50 EmptyLine, cursor = Nothing }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetCursor c ->
            { model | cursor = c }

        Nop ->
            model


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDragEnter : Msg -> Attribute Msg
onDragEnter message =
    on "dragenter" (Decode.succeed message)


onDragLeave : Attribute Msg
onDragLeave =
    on "dragleave" (Decode.succeed (SetCursor Nothing))


onDragOver : Attribute Msg
onDragOver =
    preventDefaultOn "dragover" (Decode.map alwaysPreventDefault (Decode.succeed Nop))


onDrop : Attribute Msg
onDrop =
    preventDefaultOn "drop" (Decode.map alwaysPreventDefault (Decode.succeed (SetCursor (Just 9000))))


onDragEnd : Attribute Msg
onDragEnd =
    on "dragleave" (Decode.succeed (SetCursor Nothing))



-- VIEW


view : Model -> Html Msg
view { ast, cursor } =
    div []
        [ section [ id "code" ]
            (List.indexedMap
                (\i x ->
                    div
                        [ onDragEnter (SetCursor (Just i))
                        , onDragOver
                        , onDrop

                        --, onDragEnd
                        , class "line"
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
        , section [ id "messages" ] [ text (Maybe.withDefault 0 cursor |> String.fromInt) ]
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
