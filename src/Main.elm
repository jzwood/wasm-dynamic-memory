module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Attribute, Html, aside, button, div, input, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml, updateAt)
import Utils exposing (..)


init_rows =
    30



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Position =
    ( Float, Float )



-- MODEL


type alias Model =
    { ast : List Instr, cursor : Maybe Cursor, dragged : { instr : Instr, pos : Position, origin : Maybe Cursor }, message : String }


type Msg
    = SetCursor (Maybe Cursor)
    | OnUp Cursor
    | UpdateArg1 Cursor String
    | UpdateArg2 Cursor String
    | OnDown Instr (Maybe Cursor)
    | OnMove Position
    | Nop


init : Model
init =
    { ast = Else :: List.repeat init_rows EmptyLine, cursor = Nothing, message = "", dragged = { instr = EmptyLine, pos = ( 0, 0 ), origin = Nothing } }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { cursor, dragged, ast } =
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

        OnDown i c ->
            let
                meta =
                    getMeta i
            in
            { model | cursor = Nothing, dragged = { instr = i, pos = ( 0, 0 ), origin = c }, message = String.concat [ "(", meta.button, ") ", meta.docs ] }

        OnMove pos ->
            { model | dragged = { dragged | pos = pos } }

        OnUp c1 ->
            case ( dragged.instr, dragged.pos, dragged.origin ) of
                ( instr, _, Nothing ) ->
                    { model | ast = insert instr c1 ast, cursor = Nothing }

                ( i, _, Just c0 ) ->
                    let
                        offset =
                            if c0 < c1 then
                                -1

                            else
                                0
                    in
                    { model | ast = remove c0 ast |> insert i (c1 + offset), cursor = Nothing }

        Nop ->
            model


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDown : Msg -> Attribute Msg
onDown msg =
    Pointer.onDown (\event -> log "DOWN" msg)


onUp : Cursor -> Attribute Msg
onUp cursor =
    Pointer.onUp (\event -> log "UP" (OnUp cursor))


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


onMove : Attribute Msg
onMove =
    Touch.onWithOptions "touchmove"
        { stopPropagation = False, preventDefault = False }
        (\event -> OnMove <| touchCoordinates event)



--Pointer.onMove (\event -> OnMove (log "MOVE" event.pointer.clientPos))
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
                    [ --onEnter (SetCursor (Just line))
                      onUp line
                    , --onDown (Down instr (Just line))
                      style "padding-left" (String.fromInt (i * 2) ++ "ch")
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
view { ast, cursor, message, dragged } =
    let
        { pos, instr } =
            dragged

        meta =
            getMeta instr
    in
    div [ onMove ]
        [ section [ id "code" ] (astToHtml cursor ast)
        , section [ id "messages" ] [ text message ]
        , section [ id "instructions" ] (toHtml instructions)
        , div
            [ id "paddle"
            , style "position" "absolute"
            , style "pointer-events" "none"
            , style "user-select" "none"
            , style "left" ((pos |> Tuple.first |> String.fromFloat) ++ "px")
            , style "top" ((pos |> Tuple.second |> String.fromFloat) ++ "px")
            , class "instr"
            ]
            [ text meta.button ]
        ]


toHtml : List Instr -> List (Html Msg)
toHtml ins =
    List.map
        (\instr ->
            let
                meta =
                    getMeta instr
            in
            div
                [ style "user-select" "none"
                , style "touch-action" "pan-x"
                , class "instr"
                , class (meta.class ++ "-border")
                , onDown (OnDown instr Nothing)
                ]
                [ meta.button |> text ]
        )
        ins
