module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (Attribute, Html, aside, button, div, input, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml, updateAt)
import Task
import Utils exposing (..)


init_rows : Int
init_rows =
    30


line_height : number
line_height =
    23



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Position =
    ( Float, Float )



-- MODEL


type alias Dragged =
    { instr : Instr, pos : Position, origin : Maybe Cursor }


type alias Model =
    { ast : List Instr, dragged : Maybe Dragged, message : String, viewport : Dom.Viewport }


type Msg
    = OnUp Cursor
    | UpdateArg1 Cursor String
    | UpdateArg2 Cursor String
    | OnDown Instr (Maybe Cursor) Position
    | OnMove Position
    | ResetDragged
    | SetViewport Dom.Viewport
    | Nop


initViewport : Dom.Viewport
initViewport =
    { scene = { width = 0, height = 0 }, viewport = { x = 0, y = 0, width = 0, height = 0 } }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ast = Else :: List.repeat init_rows EmptyLine, message = "", dragged = Nothing, viewport = initViewport }, cmdViewport )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { ast } =
            model
    in
    case msg of
        SetViewport viewport ->
            ( { model | viewport = viewport }, Cmd.none )

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
            ( { model | ast = updateAt c updateInstr ast }, Cmd.none )

        --{ model | ast = replace (Num n) c ast }
        UpdateArg2 c n ->
            ( model, Cmd.none )

        OnDown i c pos ->
            let
                meta =
                    getMeta i
            in
            ( { model | dragged = Just { instr = i, pos = pos, origin = c }, message = String.concat [ "(", meta.button, ") ", meta.docs ] }, cmdViewport )

        OnMove pos ->
            case model.dragged of
                Just dragged ->
                    ( { model | dragged = Just { dragged | pos = pos } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OnUp c1 ->
            case model.dragged of
                Nothing ->
                    ( model, Cmd.none )

                Just dragged ->
                    case ( dragged.instr, dragged.origin ) of
                        ( instr, Nothing ) ->
                            ( { model | ast = insert instr c1 ast, dragged = Nothing }, Cmd.none )

                        ( instr, Just c0 ) ->
                            let
                                postRemoveCursor =
                                    c1
                                        - (if c0 < c1 then
                                            1

                                           else
                                            0
                                          )
                            in
                            ( { model | ast = remove c0 ast |> insert instr postRemoveCursor, dragged = Nothing }, Cmd.none )

        ResetDragged ->
            ( { model | dragged = Nothing }, Cmd.none )

        Nop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


onDown : (Position -> Msg) -> Attribute Msg
onDown fxn =
    Pointer.onDown
        (\event -> fxn event.pointer.clientPos)


onUp : Maybe Cursor -> Attribute Msg
onUp maybeCursor =
    Pointer.onUp
        (\event ->
            case maybeCursor of
                Nothing ->
                    Nop

                Just cursor ->
                    OnUp cursor
        )


onContextmenu : Attribute Msg
onContextmenu =
    on "oncontextmenu" (Decode.succeed Nop)


onPointerCancel : Attribute Msg
onPointerCancel =
    Pointer.onCancel (\event -> ResetDragged)


onPointerOut : Attribute Msg
onPointerOut =
    Pointer.onOut (\event -> ResetDragged)


onPointerMove : Attribute Msg
onPointerMove =
    Pointer.onWithOptions "pointermove"
        { stopPropagation = False, preventDefault = False }
        (\event -> OnMove event.pointer.clientPos)



--onPointerLeave : Attribute Msg
--onPointerLeave = Pointer.onLeave (\event -> OnMove (0, 0))
--onScroll : Attribute Msg
--onScroll =
--on "scroll" (Decode.succeed OnScroll)


cmdViewport : Cmd Msg
cmdViewport =
    Task.attempt
        (\result ->
            case result of
                Ok viewport ->
                    SetViewport viewport

                Err _ ->
                    Nop
        )
        (Dom.getViewportOf "code")



--touchCoordinates : Touch.Event -> ( Float, Float )
--touchCoordinates touchEvent =
--List.head touchEvent.changedTouches
--|> Maybe.map .clientPos
--|> Maybe.withDefault ( 0, 0 )
--onTouchMove : Attribute Msg
--onTouchMove =
--Touch.onWithOptions "touchmove"
--{ stopPropagation = False, preventDefault = False }
--(\event -> OnMove <| touchCoordinates event)
--onMouseMove : Attribute Msg
--onMouseMove =
--Mouse.onWithOptions "mousemove"
--{ stopPropagation = False, preventDefault = False }
--(\event -> OnMove event.clientPos)
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
                    [ style "padding-left" (String.fromInt (i * 2) ++ "ch")
                    , style "height" <| String.fromInt line_height ++ "px"
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
                    [ span
                        [ onDown (OnDown instr (Just line))
                        , onPointerCancel
                        , onPointerOut
                        , onUp cursor
                        , class "line-instr"
                        ]
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

                --Get v ->
                --( ( c, nextLine, indent )
                --, div (attrs indent)
                --[ span [] [ text meta.button ]
                --, span [ class "input", style "margin-left" "1ch" ] [ text "$" ]
                --, input
                --[ value v
                --, class "input"
                --, attribute "type" "text"
                --, onInput (UpdateArg1 line)
                --]
                --[]
                --]
                --)
                --Num n ->
                --( ( c, nextLine, indent )
                --, div (attrs indent)
                --[ input
                --[ value (String.fromInt n)
                --, class "input"
                --, attribute "type" "text"
                --, onInput (UpdateArg1 line)
                --]
                --[]
                --]
                --)
                op ->
                    neutralLine
        )
        ( cursor, 0, 0 )
        ast
        |> Tuple.second


view : Model -> Html Msg
view { ast, message, dragged, viewport } =
    let
        cursor =
            dragged
                |> Maybe.andThen
                    (\{ pos } ->
                        let
                            ( x, y ) =
                                pos
                        in
                        if x > 0 && y > 0 && y < viewport.viewport.height then
                            Just ((y + viewport.viewport.y) / line_height |> round)

                        else
                            Nothing
                    )
    in
    div [ onPointerMove ]
        [ section
            [ id "code"
            , preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed Nop))
            , style "overflow"
                (case dragged of
                    Just _ ->
                        "hidden"

                    Nothing ->
                        ""
                )
            ]
            (astToHtml cursor ast)
        , section [ id "messages" ] [ text message ]
        , section [ id "instructions" ] (instrToHtml cursor instructions)
        , viewPaddle dragged
        ]


viewPaddle : Maybe Dragged -> Html Msg
viewPaddle dragged =
    case dragged of
        Nothing ->
            text ""

        Just { instr, pos, origin } ->
            let
                meta =
                    getMeta instr
            in
            div
                [ id "paddle"
                , style "left" ((pos |> Tuple.first |> String.fromFloat) ++ "px")
                , style "top" ((pos |> Tuple.second |> String.fromFloat) ++ "px")
                , class "instr"
                ]
                [ text meta.button ]


instrToHtml : Maybe Cursor -> List Instr -> List (Html Msg)
instrToHtml cursor ins =
    List.map
        (\instr ->
            let
                meta =
                    getMeta instr
            in
            div
                [ class "instr"
                , class (meta.class ++ "-border")
                , onDown (OnDown instr Nothing)
                , onUp cursor
                , onPointerCancel
                , onPointerOut
                ]
                [ meta.button |> text ]
        )
        ins
