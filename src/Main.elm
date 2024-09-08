module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, input, section, text)
import Html.Attributes exposing (attribute, class, draggable, id, style, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
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
    { ast : List Instr, cursor : Maybe Cursor, instr : ( Instr, Maybe Cursor ), message : String }


type Msg
    = SetCursor (Maybe Cursor)
    | InsertInstr Cursor
    | UpdateNum Cursor Int
    | SetDragging Instr (Maybe Cursor)
    | Nop


init : Model
init =
    { ast = List.repeat init_rows EmptyLine, cursor = Nothing, message = "", instr = ( EmptyLine, Nothing ) }



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

        UpdateNum c n ->
            { model | ast = replace (Num n) c ast }

        InsertInstr c1 ->
            case instr of
                ( i, Nothing ) ->
                    { model | ast = insert i c1 ast, cursor = Nothing }

                ( i, Just c0 ) ->
                    let
                        offset =
                            if c0 < c1 then
                                -1

                            else
                                0
                    in
                    { model | ast = remove c0 ast |> insert i (c1 + offset), cursor = Nothing }

        SetDragging i c ->
            let
                meta =
                    instr |> Tuple.first |> getMeta
            in
            { model | cursor = Nothing, instr = ( i, c ), message = String.concat [ "(", meta.button, ") ", meta.docs ] }

        Nop ->
            model


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    on "dragenter" (Decode.succeed msg)


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


changeNum : Cursor -> String -> Msg
changeNum line num =
    String.filter Char.isDigit num |> String.toInt |> Maybe.withDefault 0 |> UpdateNum line



-- VIEW


astToHtml : Maybe Cursor -> List Instr -> List (Html Msg)
astToHtml cursor ast =
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
                    , onDragStart (SetDragging instr (Just line))
                    , draggable "true"
                    , style "padding-left" (String.fromInt (i * 2) ++ "ch")
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
                    [ case instr of
                        Block l ca ->
                            unwords [ meta.button, "$" ++ l, "+" ++ String.fromInt ca ] |> text

                        Loop l ca ->
                            unwords [ meta.button, "$" ++ l, "+" ++ String.fromInt ca ] |> text

                        If l ca ->
                            unwords [ meta.button, "$" ++ l, "+" ++ String.fromInt ca ] |> text

                        _ ->
                            meta.button |> text
                    ]

                indentedLine =
                    ( ( c, nextLine, indent + 1 ), div (attrs indent) body )

                neutralLine =
                    ( ( c, nextLine, indent ), div (attrs indent) body )
            in
            case instr of
                Fun _ _ _ ->
                    indentedLine

                Loop _ _ ->
                    indentedLine

                If _ _ ->
                    indentedLine

                Block _ _ ->
                    indentedLine

                End ->
                    let
                        nextIndent =
                            max 0 (indent - 1)
                    in
                    ( ( c, nextLine, nextIndent ), div (attrs nextIndent) body )

                Num n ->
                    ( ( c, nextLine, indent )
                    , div (attrs indent)
                        [ input
                            [ value (String.fromInt n)
                            , style "margin-left" "0.25rem"
                            , attribute "type" "text"
                            , style "background-color" "transparent"
                            , onInput (changeNum line)
                            ]
                            []
                        ]
                    )

                op ->
                    neutralLine
        )
        ( cursor, 0, 0 )
        ast
        |> Tuple.second


view : Model -> Html Msg
view { ast, cursor, message } =
    div []
        [ section [ id "code" ] (astToHtml cursor ast)
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
            div [ class "instr", class (meta.class ++ "-border"), draggable "true", onDragStart (SetDragging instr Nothing) ] [ meta.button |> text ]
        )
        ins
