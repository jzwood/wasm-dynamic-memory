module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, input, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml, updateAt)
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
    | UpdateArg1 Cursor String
    | UpdateArg2 Cursor String
    | StartDragging Instr (Maybe Cursor)
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

        UpdateArg1 c v ->
            let
                updateInstr : Instr -> Instr
                updateInstr i =
                    case i of
                        Num n ->
                            String.filter Char.isDigit v |> String.toInt |> Maybe.withDefault 0 |> Num

                        Let _ ->
                            Let v

                        Set _ ->
                            Set v

                        Get _ ->
                            Get v

                        Br _ ->
                            Br v

                        BrIf _ ->
                            BrIf v

                        Call _ ->
                            Call v

                        Fun _ a ca ->
                            Fun v a ca

                        Block _ ca ->
                            Block v ca

                        Loop _ ca ->
                            Loop v ca

                        If _ ca ->
                            If v ca

                        _ ->
                            i
            in
            { model | ast = updateAt c updateInstr ast }

        --{ model | ast = replace (Num n) c ast }
        UpdateArg2 c n ->
            model

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

        StartDragging i c ->
            let
                meta =
                    getMeta i
            in
            { model | cursor = Nothing, instr = ( i, c ), message = String.concat [ "(", meta.button, ") ", meta.docs ] }

        Nop ->
            model


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    --on "mousedown" (Decode.succeed msg)
    preventDefaultOn "mousedown" (Decode.map alwaysPreventDefault (Decode.succeed msg))


onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    on "mouseenter" (Decode.succeed msg)


onDragOver : Attribute Msg
onDragOver =
    preventDefaultOn "mouseover" (Decode.map alwaysPreventDefault (Decode.succeed Nop))


onDrop : Cursor -> Attribute Msg
onDrop cursor =
    preventDefaultOn "mouseup" (Decode.map alwaysPreventDefault (Decode.succeed (InsertInstr cursor)))


onDragEnd : Attribute Msg
onDragEnd =
    on "mouseout" (Decode.succeed (SetCursor Nothing))



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
                    , onDragStart (StartDragging instr (Just line))
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

                Get v ->
                    ( ( c, nextLine, indent )
                    , div (attrs indent)
                        [ span [] [ text meta.button ]
                        , span [ class "input", style "margin-left" "1ch" ] [ text "$" ]
                        , input
                            [ value v
                            , class "input"
                            , attribute "type" "text"
                            , onInput (UpdateArg1 line)
                            ]
                            []
                        ]
                    )

                Num n ->
                    ( ( c, nextLine, indent )
                    , div (attrs indent)
                        [ input
                            [ value (String.fromInt n)
                            , class "input"
                            , attribute "type" "text"
                            , onInput (UpdateArg1 line)
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
            div [ class "instr", class (meta.class ++ "-border"), onDragStart (StartDragging instr Nothing) ] [ meta.button |> text ]
        )
        ins
