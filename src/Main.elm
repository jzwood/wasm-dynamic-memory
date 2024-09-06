module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml)
import Utils exposing (..)


init_rows =
    30



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { ast : List Instr, cursor : Maybe Cursor, instr : Instr, message : String }


type Msg
    = SetCursor (Maybe Cursor)
    | InsertInstr Cursor
    | SetDragging Instr
    | Nop


init : Model
init =
    { ast = List.repeat init_rows EmptyLine, cursor = Nothing, message = "", instr = EmptyLine }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { cursor, instr, ast } =
            model
    in
    case msg of
        SetCursor c ->
            { model | cursor = c }

        InsertInstr c ->
            { model | ast = insert instr c ast, cursor = Nothing }

        SetDragging i ->
            let
                meta =
                    getMeta instr
            in
            { model | cursor = Nothing, instr = i, message = meta.button ++ ": " ++ meta.docs }

        Nop ->
            model


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDragStart : Instr -> Attribute Msg
onDragStart instr =
    on "dragstart" (Decode.succeed (SetDragging instr))


onDragEnter : Msg -> Attribute Msg
onDragEnter message =
    on "dragenter" (Decode.succeed message)


onDragLeave : Attribute Msg
onDragLeave =
    on "dragleave" (Decode.succeed (SetCursor Nothing))


onDragOver : Attribute Msg
onDragOver =
    preventDefaultOn "dragover" (Decode.map alwaysPreventDefault (Decode.succeed Nop))


onDrop : Cursor -> Attribute Msg
onDrop cursor =
    preventDefaultOn "drop" (Decode.map alwaysPreventDefault (Decode.succeed (InsertInstr cursor)))


onDragEnd : Attribute Msg
onDragEnd =
    on "dragend" (Decode.succeed (SetCursor Nothing))



-- VIEW


astToHtml2 : Maybe Cursor -> List Instr -> List (Html Msg)
astToHtml2 cursor ast =
    List.Extra.mapAccuml
        (\( c, line, indent ) instr ->
            let
                meta =
                    getMeta instr

                nextLine =
                    line + 1

                attrs : Int -> List (Attribute Msg)
                attrs i =
                    [ onDragEnter (SetCursor (Just line))
                    , onDragOver
                    , onDrop line
                    , style "margin-left" (String.fromInt (i * 2) ++ "ch")
                    , class "line"
                    , class
                        (if cursor == Just line then
                            "cursor"

                         else
                            ""
                        )
                    , class meta.class
                    ]

                body : List (Html Msg)
                body =
                    [ meta.button |> text ]
            in
            case instr of
                Block _ _ ->
                    ( ( c, nextLine, indent + 1 ), div (attrs indent) body )

                End ->
                    let
                        nextIndent =
                            max 0 (indent - 1)
                    in
                    ( ( c, nextLine, nextIndent ), div (attrs nextIndent) body )

                op ->
                    ( ( c, nextLine, indent ), div (attrs indent) body )
        )
        ( cursor, 0, 0 )
        ast
        |> Tuple.second


view : Model -> Html Msg
view { ast, cursor, message } =
    div []
        [ section [ id "code" ] (astToHtml2 cursor ast)
        , section [ id "messages" ] [ text message ]
        , section [ id "instructions" ] (toHtml instructions)
        ]


toHtml : List Instr -> List (Html Msg)
toHtml ins =
    List.map
        (\instr ->
            let
                meta =
                    getMeta instr
            in
            div [ class "instr", class (meta.class ++ "-border"), draggable "true", onDragStart instr ] [ meta.button |> text ]
        )
        ins
