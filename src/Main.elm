module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml)
import Utils exposing (..)



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
    { ast = List.repeat 20 EmptyLine, cursor = Nothing, message = "", instr = EmptyLine }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { cursor, instr, ast } =
            model
    in
    case msg of
        SetCursor c ->
            { model | cursor = c, message = Maybe.withDefault 0 cursor |> String.fromInt }

        InsertInstr c ->
            { model | ast = insert instr c ast, cursor = Nothing, message = (getMeta instr).button }

        SetDragging i ->
            { model | cursor = Nothing, instr = i }

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


astToHtml : Maybe Cursor -> List Instr -> List (Html Msg)
astToHtml cursor ast =
    List.indexedMap
        (\i instr ->
            let
                meta =
                    getMeta instr

                attrs =
                    [ onDragEnter (SetCursor (Just i))
                    , onDragOver
                    , onDrop i
                    , class "line"
                    , class meta.class
                    , class
                        (if cursor == Just i then
                            "cursor"

                         else
                            ""
                        )
                    ]

                innerHtml =
                    [ meta.button |> text ]
            in
            div attrs innerHtml
        )
        ast



-- ((Maybe Cursor, Indent) -> Instr -> ((Maybe Cursor, Indent), Html Msg )) -> (Maybe Cursor, Indent) -> List Instr -> ((Maybe Cursor, Indent), List (Html Msg) )


astToHtml2 : Maybe Cursor -> List Instr -> List (Html Msg)
astToHtml2 cursor ast =
    List.Extra.mapAccuml
        (\( c, line, indent ) instr ->
            let
                meta =
                    getMeta instr

                nextLine =
                    line + 1

                attrs =
                    [ onDragEnter (SetCursor (Just line))
                    , onDragOver
                    , onDrop line
                    , class "line"
                    , class
                        (if cursor == Just line then
                            "cursor"

                         else
                            ""
                        )
                    , class meta.class
                    ]

                result attribs =
                    ( ( c, line + 1, 0 ), div attribs [ meta.button |> text ] )
            in
            result
                (case instr of
                    Block _ _ ->
                        attrs

                    End ->
                        attrs

                    op ->
                        attrs
                )
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
    List.map (\instr -> div [ class "instr", draggable "true", onDragStart instr ] [ (getMeta instr).button |> text ]) ins



--ins
--|> groupWhile (\(Instr a) (Instr b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instr i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
