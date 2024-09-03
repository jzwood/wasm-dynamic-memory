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
    List Instruction


type Signature
    = Sig (List Type) Result



-- foldl : (a -> b -> b) -> b -> List a -> b
--insert : (Instruction -> (Cursor, List Instruction) -> (Cursor, List Instruction)) -> (Cursor, List Instruction) -> List Instruction -> (Cursor, List Instruction)
--insert : Instruction -> (Cursor, List Instruction) -> (Cursor, List Instruction)
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


insert : Instruction -> Cursor -> Cursor -> List Instruction -> ( List Instruction, Cursor )
insert instr cursor line ast =
    if cursor == line then
        ( instr :: ast, line + 1 )

    else
        case ast of
            [] ->
                ( ast, line )

            i :: is ->
                let
                    innerInsert : List Instruction -> ( List Instruction, Cursor )
                    innerInsert =
                        insert instr cursor (line + 1)
                in
                case i of
                    Fun n s children ->
                        let
                            ( nextChildren, nextLine ) =
                                innerInsert children
                        in
                        ( Fun n s nextChildren :: is, nextLine )

                    Block l r children ->
                        let
                            ( nextChildren, nextLine ) =
                                innerInsert children
                        in
                        ( Block l r nextChildren :: is, nextLine )

                    Loop l r children ->
                        let
                            ( nextChildren, nextLine ) =
                                innerInsert children
                        in
                        ( Loop l r nextChildren :: is, nextLine )

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


type Instruction
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
    | Else Children
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


type Instr
    = Instr { instr : Instruction, button : String, docs : Docs }


instructions : List Instr
instructions =
    [ Instr { instr = Add, button = "+", docs = "addition" }
    , Instr { instr = Block "" [] [], button = "block", docs = "breaking to block label jumps out of block" }
    , Instr { instr = Loop "" [] [], button = "loop", docs = "breaking to loop label jumps to top of loop" }
    , Instr { instr = If "" [] [] [], button = "if", docs = "breaking to if label jumps out of if" }
    , Instr { instr = Else [], button = "else", docs = "else branch of if statement" }
    , Instr { instr = Add, button = "+", docs = "addition" }
    , Instr { instr = Sub, button = "sub", docs = "subtraction" }
    , Instr { instr = Mul, button = "mult", docs = "multiplication" }
    , Instr { instr = Div, button = "div", docs = "division" }
    , Instr { instr = Rem, button = "%", docs = "remainder" }
    , Instr { instr = Gt, button = ">", docs = "greater than" }
    , Instr { instr = Gte, button = "≥", docs = "greater than or equal" }
    , Instr { instr = Lt, button = "<", docs = "less than" }
    , Instr { instr = Lte, button = "≤", docs = "less than or equal" }
    , Instr { instr = Eq, button = "=", docs = "equal" }
    , Instr { instr = Neq, button = "≠", docs = "not equal" }
    , Instr { instr = And, button = "and", docs = "bitwise/logical and" }
    , Instr { instr = Or, button = "or", docs = "bitwise/logical or" }
    , Instr { instr = Xor, button = "xor", docs = "bitwise/logical xor" }
    , Instr { instr = Rsh, button = "»", docs = "bitwise right shift" }
    , Instr { instr = Lsh, button = "«", docs = "bitwise left shift" }
    , Instr { instr = Num 0, button = "num", docs = "constant number" }
    , Instr { instr = Fun "" (Sig [] []) [], button = "f(x)", docs = "function definition" }
    , Instr { instr = Let "", button = "let", docs = "local variable declaration" }
    , Instr { instr = Set "", button = "set", docs = "set local variable" }
    , Instr { instr = Get "", button = "get", docs = "get local variable" }
    , Instr { instr = Br "", button = "br", docs = "break to label" }
    , Instr { instr = BrIf "", button = "br_if", docs = "if truthy break to label" }
    , Instr { instr = Return, button = "return", docs = "return function" }
    , Instr { instr = Call "", button = "call", docs = "call function" }
    , Instr { instr = Nop, button = "nop", docs = "no operation" }
    , Instr { instr = Drop, button = "drop", docs = "drop top of stack" }
    , Instr { instr = Malloc, button = "malloc", docs = "allocate n bytes" }
    , Instr { instr = Read1, button = "read1", docs = "read 1 bytee" }
    , Instr { instr = Read2, button = "read2", docs = "read 2 bytes" }
    , Instr { instr = Read4, button = "read4", docs = "read 4 bytes" }
    , Instr { instr = Write1, button = "write1", docs = "write 1 byte" }
    , Instr { instr = Write2, button = "write2", docs = "write 2 bytes" }
    , Instr { instr = Write4, button = "write4", docs = "write 4 bytes" }
    ]
