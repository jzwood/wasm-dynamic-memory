module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (Attribute, Html, aside, button, div, header, input, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Icons exposing (..)
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml, removeAt, updateAt)
import Task
import Typecheck exposing (Typecheck, typecheck)
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


type Mode
    = Code
    | WAT



-- MODEL


type alias Dragged =
    { instr : Instr, pos : Position, origin : Maybe Cursor }


type alias CodeDom =
    { scrollTop : Float, top : Float, left : Float, width : Float, height : Float }


type alias Model =
    { mode : Mode, ast : List Instr, dragged : Maybe Dragged, message : String, dom : CodeDom }


type Msg
    = OnUp Cursor
    | UpdateArg1 Cursor String
    | OnDown Instr (Maybe Cursor) Position
    | OnMove Position
    | ResetDragged
    | SetDom CodeDom
    | ToggleMode
    | Nop


initDom : CodeDom
initDom =
    { scrollTop = 0, top = 0, left = 0, width = 0, height = 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mode = Code, ast = List.repeat init_rows EmptyLine, message = "", dragged = Nothing, dom = initDom }, cmdViewport )


toggleMode : Mode -> Mode
toggleMode mode =
    case mode of
        Code ->
            WAT

        WAT ->
            Code



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { ast, mode } =
            model
    in
    case msg of
        SetDom dom ->
            ( { model | dom = dom }, Cmd.none )

        UpdateArg1 c v ->
            let
                updateInstr : Instr -> Instr
                updateInstr i =
                    case i of
                        Num _ ->
                            strToInt v |> Num

                        Result _ ->
                            strToInt v |> Result

                        Arg _ ->
                            Arg v

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

                        Fun _ ->
                            Fun v

                        Block _ ->
                            Block v

                        Loop _ ->
                            Loop v

                        If _ ->
                            If v

                        _ ->
                            i
            in
            ( { model | ast = updateAt c updateInstr ast }, Cmd.none )

        OnDown i c pos ->
            let
                meta =
                    getMeta i
            in
            ( { model | dragged = Just { instr = i, pos = pos, origin = c }, message = meta.docs }, cmdViewport )

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
                        ( DEL, _ ) ->
                            ( { model | ast = removeAt c1 ast, dragged = Nothing }, Cmd.none )

                        ( instr, Nothing ) ->
                            ( { model | ast = insert instr c1 ast, dragged = Nothing }, Cmd.none )

                        ( instr, Just c0 ) ->
                            ( { model | ast = move ( instr, c0 ) c1 ast, dragged = Nothing }, Cmd.none )

        ResetDragged ->
            ( { model | dragged = Nothing }, Cmd.none )

        ToggleMode ->
            ( { model | mode = toggleMode mode }, Cmd.none )

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
        (\event -> log "UP" (Maybe.map OnUp maybeCursor |> Maybe.withDefault Nop))


onContextmenu : Attribute Msg
onContextmenu =
    preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed Nop))


onPointerCancel : Attribute Msg
onPointerCancel =
    Pointer.onCancel (\event -> log "CANCEL" ResetDragged)


onPointerMove : Attribute Msg
onPointerMove =
    Pointer.onWithOptions "pointermove"
        { stopPropagation = False, preventDefault = False }
        (\event -> OnMove event.pointer.clientPos)


captureDown : Attribute Msg
captureDown =
    Pointer.onWithOptions "pointerdown"
        { stopPropagation = True, preventDefault = False }
        (\event -> Nop)



--onPointerLeave : Attribute Msg
--onPointerLeave = Pointer.onLeave (\event -> OnMove (0, 0))
--onScroll : Attribute Msg
--onScroll =
--on "scroll" (Decode.succeed OnScroll)


cmdViewport : Cmd Msg
cmdViewport =
    Task.map2
        (\{ viewport } { element } -> { scrollTop = viewport.y, top = element.y, left = element.x, width = element.width, height = element.height })
        (Dom.getViewportOf "code")
        (Dom.getElement "code")
        |> Task.attempt
            (\result ->
                case result of
                    Ok dom ->
                        SetDom dom

                    Err _ ->
                        Nop
            )



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


inputNode : List (Attribute Msg) -> String -> (String -> Msg) -> Html Msg
inputNode attrs val oninput =
    input
        ([ value val
         , class "input"
         , attribute "size" (val |> String.length |> max 1 |> String.fromInt)
         , onInput oninput
         , captureDown
         ]
            ++ attrs
        )
        []


textInput : String -> (String -> Msg) -> Html Msg
textInput =
    inputNode [ attribute "type" "text" ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput =
    inputNode [ attribute "type" "number", attribute "min" "0" ]


indentLines : (Int -> Cursor -> Instr -> Html Msg) -> List Instr -> List (Html Msg)
indentLines instrToHtml =
    List.Extra.mapAccuml
        (\( line, indent ) instr ->
            let
                nextLine =
                    line + 1

                indentedLine =
                    ( ( nextLine, indent + 1 ), instrToHtml line indent instr )

                neutralLine =
                    ( ( nextLine, indent ), instrToHtml line indent instr )

                unindentedLine =
                    ( ( nextLine, indent - 1 ), instrToHtml line (indent - 1) instr )
            in
            case instr of
                Fun _ ->
                    indentedLine

                Loop _ ->
                    indentedLine

                If _ ->
                    indentedLine

                Block _ ->
                    indentedLine

                End ->
                    unindentedLine

                op ->
                    neutralLine
        )
        ( 0, 0 )
        >> Tuple.second


astToHtml : Mode -> Maybe Cursor -> List Instr -> List (Html Msg)
astToHtml mode cursor ast =
    let
        check : Typecheck
        check =
            typecheck ast
    in
    case mode of
        Code ->
            indentLines (astToCode cursor check) ast

        WAT ->
            [ div [ id "wat" ]
                [ div [] [ text "(module" ]
                , div [ class "ml2" ] (indentLines astToWat ast)
                , div [] [ text ")" ]
                ]
            ]


astToWat : Cursor -> Int -> Instr -> Html Msg
astToWat line indent instr =
    let
        meta =
            getMeta instr
    in
    div [ class "pre-wrap" ]
        [ case instr of
            _ ->
                text meta.wat
        ]


astToCode : Maybe Cursor -> Typecheck -> Cursor -> Int -> Instr -> Html Msg
astToCode cursor check line indent instr =
    let
        meta =
            getMeta instr

        nextLine =
            line + 1

        cursorClass =
            if cursor == Just line then
                "cursor"

            else
                ""

        warning : Html Msg
        warning =
            div [] []

        body : Int -> List (Html Msg) -> Html Msg
        body i innerHtml =
            div
                [ onUp cursor
                , style "height" <| String.fromInt line_height ++ "px"
                , class "line"
                , class meta.class
                , class cursorClass
                ]
                [ span [ class "line-number" ] [ text <| String.fromInt line ]
                , div
                    [ onDown (OnDown instr (Just line))
                    , onPointerCancel
                    , class "line-instr"
                    , style "margin-left" (String.fromInt (i * 2) ++ "ch")
                    ]
                    innerHtml
                , div [ style "flex-grow" "1" ] [ warning ]
                ]

        var1 : Variable -> Html Msg
        var1 v =
            body indent
                [ span [ class "mr1" ] [ text meta.button ]
                , textInput (strToLabel v) (UpdateArg1 line)
                ]

        num1 : Int -> Html Msg
        num1 n =
            body indent
                [ span [ class "mr1" ] [ text meta.button ]
                , textInput (String.fromInt n) (UpdateArg1 line)
                ]
    in
    case instr of
        Fun v ->
            var1 v

        Loop v ->
            var1 v

        If v ->
            var1 v

        Block v ->
            var1 v

        End ->
            body indent [ text meta.button ]

        Arg v ->
            var1 v

        Result n ->
            num1 n

        Let v ->
            var1 v

        Get v ->
            var1 v

        Set v ->
            var1 v

        Br v ->
            var1 v

        BrIf v ->
            var1 v

        Call v ->
            var1 v

        Num n ->
            num1 n

        op ->
            body indent <| [ text meta.button ]


withinBounds : Position -> CodeDom -> Bool
withinBounds ( x, y ) { scrollTop, top, left, width, height } =
    ordered left x (left + width) && ordered top y (top + height)


toCursor : Position -> CodeDom -> Cursor
toCursor ( _, y ) { scrollTop, top, left, width, height } =
    (y - top + scrollTop) / line_height |> round


view : Model -> Html Msg
view { ast, message, dragged, dom, mode } =
    let
        cursor =
            dragged
                |> Maybe.andThen
                    (\{ pos } ->
                        let
                            ( x, y ) =
                                pos
                        in
                        if withinBounds pos dom then
                            Just (toCursor pos dom)

                        else
                            Nothing
                    )
    in
    div [ onPointerMove ]
        [ header []
            [ button [ class "wat", onClick ToggleMode ]
                [ if mode == Code then
                    eye

                  else
                    eyeSlash
                , span [ class "ml1" ] [ text "WAT" ]
                ]
            ]
        , section
            [ id "code"
            , onContextmenu
            , style "overflow"
                (case dragged of
                    Just _ ->
                        "hidden"

                    Nothing ->
                        ""
                )
            ]
            (astToHtml mode cursor ast)
        , section [ id "messages" ] [ text message ]
        , section [ id "instructions" ] (menuInstrToHtml cursor instructions)
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


menuInstrToHtml : Maybe Cursor -> List Instr -> List (Html Msg)
menuInstrToHtml cursor ins =
    List.map
        (\instr ->
            let
                meta =
                    getMeta instr
            in
            div
                [ class "instr"
                , class meta.class
                , onDown (OnDown instr Nothing)
                , onUp cursor
                , onPointerCancel
                ]
                [ meta.button |> text ]
        )
        ins
