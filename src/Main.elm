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
import List.Extra exposing (mapAccuml, removeAt, updateAt)
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


type alias CodeDom =
    { scrollTop : Float, top : Float, left : Float, width : Float, height : Float }


type alias Model =
    { ast : List Instr, dragged : Maybe Dragged, message : String, dom : CodeDom }


type Msg
    = OnUp Cursor
    | UpdateArg1 Cursor String
    | UpdateArg2 Cursor String
    | OnDown Instr (Maybe Cursor) Position
    | OnMove Position
    | ResetDragged
    | SetDom CodeDom
    | Nop


initDom : CodeDom
initDom =
    { scrollTop = 0, top = 0, left = 0, width = 0, height = 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ast = List.repeat init_rows EmptyLine, message = "", dragged = Nothing, dom = initDom }, cmdViewport )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { ast } =
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

                        Fun _ ca ->
                            Fun v ca

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

        UpdateArg2 c n ->
            let
                ca =
                    strToInt n

                updateInstr : Instr -> Instr
                updateInstr i =
                    case i of
                        Fun v _ ->
                            Fun v ca

                        Block v _ ->
                            Block v ca

                        Loop v _ ->
                            Loop v ca

                        If v _ ->
                            If v ca

                        _ ->
                            i
            in
            ( { model | ast = updateAt c updateInstr ast }, Cmd.none )

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
                        ( DEL, _ ) ->
                            ( { model | ast = removeAt c1 ast, dragged = Nothing }, Cmd.none )

                        ( instr, Nothing ) ->
                            ( { model | ast = insert instr c1 ast, dragged = Nothing }, Cmd.none )

                        ( instr, Just c0 ) ->
                            ( { model | ast = move ( instr, c0 ) c1 ast, dragged = Nothing }, Cmd.none )

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

                body : List (Html Msg) -> List (Html Msg)
                body innerHtml =
                    [ span
                        [ onDown (OnDown instr (Just line))
                        , onPointerCancel
                        , onPointerOut
                        , onUp cursor
                        , class "line-instr"
                        ]
                        innerHtml
                    ]

                indentedLine : List (Html Msg) -> ( ( Maybe Cursor, Cursor, Int ), Html Msg )
                indentedLine innerHtml =
                    ( ( c, nextLine, indent + 1 ), div (attrs indent) (body innerHtml) )

                neutralLine : List (Html Msg) -> ( ( Maybe Cursor, Cursor, Int ), Html Msg )
                neutralLine innerHtml =
                    ( ( c, nextLine, indent ), div (attrs indent) (body innerHtml) )

                unindentedLine : List (Html Msg) -> ( ( Maybe Cursor, Cursor, Int ), Html Msg )
                unindentedLine innerHtml =
                    let
                        nextIndent =
                            max 0 (indent - 1)
                    in
                    ( ( c, nextLine, nextIndent ), div (attrs nextIndent) (body innerHtml) )

                var1 : Variable -> List (Html Msg)
                var1 v =
                    [ span [ style "margin-right" "1ch" ] [ text meta.button ]
                    , textInput v (UpdateArg1 line)
                    ]

                var1ca2 : Variable -> Int -> List (Html Msg)
                var1ca2 v ca =
                    [ span [ style "margin-right" "1ch" ] [ text meta.button ]
                    , textInput v (UpdateArg1 line)
                    , span [ style "margin-left" "1ch" ] [ text "⊢" ]
                    , textInput (String.fromInt ca) (UpdateArg2 line)
                    ]
            in
            case instr of
                Fun v ca ->
                    indentedLine <| var1ca2 v ca

                Loop v ca ->
                    indentedLine <| var1ca2 v ca

                If v ca ->
                    indentedLine <| var1ca2 v ca

                Block v ca ->
                    indentedLine <| var1ca2 v ca

                End ->
                    unindentedLine <| [ text meta.button ]

                Arg v ->
                    neutralLine <| var1 v

                Let v ->
                    neutralLine <| var1 v

                Get v ->
                    neutralLine <| var1 v

                Set v ->
                    neutralLine <| var1 v

                Br v ->
                    neutralLine <| var1 v

                BrIf v ->
                    neutralLine <| var1 v

                Call v ->
                    neutralLine <| var1 v

                Num n ->
                    neutralLine
                        [ span [ style "margin-right" "1ch" ] [ text meta.button ]
                        , textInput (String.fromInt n) (UpdateArg1 line)
                        ]

                op ->
                    neutralLine <| [ text meta.button ]
        )
        ( cursor, 0, 0 )
        ast
        |> Tuple.second


withinBounds : Position -> CodeDom -> Bool
withinBounds ( x, y ) { scrollTop, top, left, width, height } =
    ordered left x (left + width) && ordered top y (top + height)


toCursor : Position -> CodeDom -> Cursor
toCursor ( _, y ) { scrollTop, top, left, width, height } =
    (y - top + scrollTop) / line_height |> round


view : Model -> Html Msg
view { ast, message, dragged, dom } =
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
                , class meta.class
                , onDown (OnDown instr Nothing)
                , onUp cursor
                , onPointerCancel
                , onPointerOut
                ]
                [ meta.button |> text ]
        )
        ins
