module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, aside, button, div, section, text)
import Html.Attributes exposing (class, draggable, id, style)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Instructions exposing (..)
import Json.Decode as Decode
import List.Extra exposing (mapAccuml)



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
    { ast = List.repeat 30 EmptyLine, cursor = Nothing, message = "", instr = EmptyLine }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { cursor, instr } =
            model
    in
    case msg of
        SetCursor c ->
            { model | cursor = c, message = Maybe.withDefault 0 cursor |> String.fromInt }

        InsertInstr c ->
            let
                ( ast, c2 ) =
                    insert instr c 0 model.ast
            in
            { model | ast = ast, cursor = Nothing, message = (getMeta instr).button }

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
            in
            div
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
                [ meta.button |> text ]
        )
        ast


instrsToHtml : Maybe Cursor -> Cursor -> List Instr -> ( ( Maybe Cursor, Cursor ), List (Html Msg) )
instrsToHtml cursor line instrs =
    mapAccuml instrToHtml ( cursor, line ) instrs


instrToHtml : ( Maybe Cursor, Cursor ) -> Instr -> ( ( Maybe Cursor, Cursor ), Html Msg )
instrToHtml ( cursor, line ) instr =
    let
        meta =
            getMeta instr

        lineClass =
            if cursor == Just line then
                "line cursor"

            else
                "line"

        lineNode : List String -> Html Msg
        lineNode labels =
            div [ class lineClass ] [ meta.button :: labels |> String.join " " |> text ]
    in
    case instr of
        Fun n s children ->
            let
                ( ( _, nextLine ), nodes ) =
                    instrsToHtml cursor line children
            in
            ( ( cursor, nextLine ), div [ class "function" ] (lineNode [ n ] :: nodes) )

        --Block l r children ->
        --applyFirst (\nextChildren -> Block l r nextChildren :: is) (innerInsert children)
        --Loop l r children ->
        --applyFirst (\nextChildren -> Loop l r nextChildren :: is) (innerInsert children)
        --If l r consequent alternative ->
        --let
        --( nextConsequent, nextLine ) =
        --innerInsert consequent
        --( nextAlternative, nextNextLine ) =
        --insert instr cursor nextLine alternative
        --in
        --( If l r nextConsequent nextAlternative :: is, nextNextLine )
        op ->
            ( ( cursor, line + 1 ), lineNode [] )


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
