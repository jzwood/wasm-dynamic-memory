module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)


type Type
    = Integer
    | Pointer
    | Bool


type alias Cursor =
    Int


type alias Result =
    List Type


type alias Label =
    String


type alias Docs =
    String


type alias Variable =
    String


type alias Children =
    List Instr


type Signature
    = Sig (List Type) Result



-- foldl : (a -> b -> b) -> b -> List a -> b
--insert : (Instr -> (Cursor, List Instr) -> (Cursor, List Instr)) -> (Cursor, List Instr) -> List Instr -> (Cursor, List Instr)
--insert : Instr -> (Cursor, List Instr) -> (Cursor, List Instr)
--insert instr (cursor, ast) =
--case instr of
--Fun _  _ children -> (cursor, ast)
--Block _ _ children -> (cursor, ast)
--Loop _ _ children -> (cursor, ast)
--If _ _ children children -> (cursor, ast)
--Else children -> (cursor, ast)
--op -> (cursor, ast)


applyFirst : (a -> b) -> ( a, c ) -> ( b, c )
applyFirst f ( x, y ) =
    ( f x, y )


insert : Instr -> Cursor -> Cursor -> Children -> ( Children, Cursor )
insert instr cursor line ast =
    if cursor == line then
        ( instr :: ast, line + 1 )
        -- @TODO change to (Maybe Cursor, List Instr) so that we can short circuit

    else
        case ast of
            [] ->
                ( ast, line + 1 )

            i :: is ->
                let
                    innerInsert : List Instr -> ( List Instr, Cursor )
                    innerInsert =
                        insert instr cursor (line + 1)
                in
                case i of
                    Fun n s children ->
                        applyFirst (\nextChildren -> Fun n s nextChildren :: is) (innerInsert children)

                    Block l r children ->
                        applyFirst (\nextChildren -> Block l r nextChildren :: is) (innerInsert children)

                    Loop l r children ->
                        applyFirst (\nextChildren -> Loop l r nextChildren :: is) (innerInsert children)

                    If l r consequent alternative ->
                        let
                            ( nextConsequent, nextLine ) =
                                innerInsert consequent

                            ( nextAlternative, nextNextLine ) =
                                insert instr cursor nextLine alternative
                        in
                        ( If l r nextConsequent nextAlternative :: is, nextNextLine )

                    op ->
                        applyFirst ((::) i) (insert instr cursor (line + 1) is)



--- Abstract Syntax Tree


type Instr
    = EmptyLine
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Gt
    | Gte
    | Lt
    | Lte
    | Eq
    | Neq
    | And
    | Or
    | Xor
    | Rsh
    | Lsh
    | Num Int
    | Fun Variable Signature Children
    | Let Variable
    | Set Variable
    | Get Variable
    | Block Label Result Children
    | Loop Label Result Children
    | If Label Result Children Children
    | Else
    | Br Label
    | BrIf Label
    | Return
    | Call Variable
    | Nop
    | Drop
    | Malloc
    | Read1
    | Read2
    | Read4
    | Write1
    | Write2
    | Write4


getMeta : Instr -> { button : String, docs : Docs, class : String }
getMeta instr =
    case instr of
        EmptyLine ->
            { button = "~", docs = "empty line", class = "empty" }

        Block _ _ _ ->
            { button = "block", docs = "breaking to block label jumps out of block", class = "control-flow" }

        Loop _ _ _ ->
            { button = "loop", docs = "breaking to loop label jumps to top of loop", class = "control-flow" }

        If _ _ _ _ ->
            { button = "if", docs = "breaking to if label jumps out of if", class = "control-flow" }

        Else ->
            { button = "else", docs = "else branch of if statement", class = "control-flow" }

        Add ->
            { button = "+", docs = "addition", class = "numeric" }

        Sub ->
            { button = "sub", docs = "subtraction", class = "numeric" }

        Mul ->
            { button = "mult", docs = "multiplication", class = "numeric" }

        Div ->
            { button = "div", docs = "division", class = "numeric" }

        Rem ->
            { button = "%", docs = "remainder", class = "numeric" }

        Gt ->
            { button = ">", docs = "greater than", class = "numeric" }

        Gte ->
            { button = "≥", docs = "greater than or equal", class = "numeric" }

        Lt ->
            { button = "<", docs = "less than", class = "numeric" }

        Lte ->
            { button = "≤", docs = "less than or equal", class = "numeric" }

        Eq ->
            { button = "=", docs = "equal", class = "numeric" }

        Neq ->
            { button = "≠", docs = "not equal", class = "numeric" }

        And ->
            { button = "and", docs = "bitwise/logical and", class = "numeric" }

        Or ->
            { button = "or", docs = "bitwise/logical or", class = "numeric" }

        Xor ->
            { button = "xor", docs = "bitwise/logical xor", class = "numeric" }

        Rsh ->
            { button = "»", docs = "bitwise right shift", class = "numeric" }

        Lsh ->
            { button = "«", docs = "bitwise left shift", class = "numeric" }

        Num _ ->
            { button = "num", docs = "constant number", class = "numeric" }

        Fun _ _ _ ->
            { button = "f(x)", docs = "function definition", class = "function" }

        Let _ ->
            { button = "let", docs = "local variable declaration", class = "variable" }

        Set _ ->
            { button = "set", docs = "set local variable", class = "variable" }

        Get _ ->
            { button = "get", docs = "get local variable", class = "variable" }

        Br _ ->
            { button = "br", docs = "break to label", class = "control-flow" }

        BrIf _ ->
            { button = "br_if", docs = "if truthy break to label", class = "control-flow" }

        Return ->
            { button = "return", docs = "return function", class = "control-flow" }

        Call _ ->
            { button = "call", docs = "call function", class = "control-flow" }

        Nop ->
            { button = "nop", docs = "no operation", class = "control-flow" }

        Drop ->
            { button = "drop", docs = "drop top of stack", class = "control-flow" }

        Malloc ->
            { button = "malloc", docs = "allocate n bytes", class = "memory" }

        Read1 ->
            { button = "read1", docs = "read 1 bytee", class = "memory" }

        Read2 ->
            { button = "read2", docs = "read 2 bytes", class = "memory" }

        Read4 ->
            { button = "read4", docs = "read 4 bytes", class = "memory" }

        Write1 ->
            { button = "write1", docs = "write 1 byte", class = "memory" }

        Write2 ->
            { button = "write2", docs = "write 2 bytes", class = "memory" }

        Write4 ->
            { button = "write4", docs = "write 4 bytes", class = "memory" }


instructions : List Instr
instructions =
    [ Add
    , Sub
    , Mul
    , Div
    , Rem
    , Gt
    , Gte
    , Lt
    , Lte
    , Eq
    , Neq
    , And
    , Or
    , Xor
    , Rsh
    , Lsh
    , Num 0
    , Fun "" (Sig [] []) [ EmptyLine ]
    , Let ""
    , Set ""
    , Get ""
    , Block "" [] [ EmptyLine ]
    , Loop "" [] [ EmptyLine ]
    , If "" [] [ EmptyLine ] []
    , Else
    , Br ""
    , BrIf ""
    , Return
    , Call ""
    , Nop
    , Drop
    , Malloc
    , Read1
    , Read2
    , Read4
    , Write1
    , Write2
    , Write4
    ]
