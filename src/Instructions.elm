module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)


type Msg
    = Increment
    | Decrement


type Type
    = Integer
    | Pointer
    | Label
    | Float
    | Bool


type Category
    = Numeric
    | Variadic
    | Memory


type Signature
    = Signature { params : List Type, results : List Type }



--- Abstract Syntax Tree


type AST
    = Module
        { globals : List GlobalLet
        , functions : List Function
        }


type GlobalLet
    = GlobalLet { docs : String, label : String }


type Function
    = Function
        { name : String
        , signature : Signature
        , body : List Instruction
        }


type Instruction
    = Op Operator
    | CF ControlFlow


type ControlFlow
    = Block
    | Loop
    | If
    | Else


type Operator
    = Add
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
    | Num
    | Fun
    | Let
    | Set
    | Get
    | Br
    | BrIf
    | Return
    | Call
    | Nop
    | Drop
    | Malloc
    | Read1
    | Read2
    | Read4
    | Write1
    | Write2
    | Write4


show : Instruction -> String
show instr =
    case instr of
        CF cf ->
            case cf of
                Block ->
                    "block"

                Loop ->
                    "loop"

                If ->
                    "if"

                Else ->
                    "else"

        Op op ->
            case op of
                Add ->
                    "+"

                Sub ->
                    "-"

                Mul ->
                    "mult"

                Div ->
                    "div"

                Rem ->
                    "%"

                Gt ->
                    ">"

                Gte ->
                    "≥"

                Lt ->
                    "<"

                Lte ->
                    "≤"

                Eq ->
                    "="

                Neq ->
                    "≠"

                And ->
                    "and"

                Or ->
                    "or"

                Xor ->
                    "xor"

                Rsh ->
                    "»"

                Lsh ->
                    "«"

                Num ->
                    "const"

                Fun ->
                    "f(x)"

                Let ->
                    "let"

                Set ->
                    "set"

                Get ->
                    "get"

                Br ->
                    "br"

                BrIf ->
                    "br_if"

                Return ->
                    "return"

                Call ->
                    "call"

                Nop ->
                    "nop"

                Drop ->
                    "drop"

                Malloc ->
                    "malloc"

                Read1 ->
                    "read1"

                Read2 ->
                    "read2"

                Read4 ->
                    "read4"

                Write1 ->
                    "write1"

                Write2 ->
                    "write2"

                Write4 ->
                    "write4"


docs : Instruction -> String
docs instr =
    case instr of
        CF cf ->
            case cf of
                Block ->
                    "block"

                Loop ->
                    "loop"

                If ->
                    "if condition"

                Else ->
                    "else"

        Op op ->
            case op of
                Add ->
                    "add"

                Sub ->
                    "subtract"

                Mul ->
                    "multiply"

                Div ->
                    "divide"

                Gt ->
                    "greater than"

                Lt ->
                    "less than"

                Gte ->
                    "greater than or equal to"

                Lte ->
                    "less than or equal to"

                Rem ->
                    "remainder"

                Eq ->
                    "equal"

                Neq ->
                    "not equal"

                And ->
                    "bitwise/logical and"

                Or ->
                    "bitwise/logical or"

                Xor ->
                    "bitwise/logical xor"

                Lsh ->
                    "left shift"

                Rsh ->
                    "right shift"

                Num ->
                    "constant number"

                Fun ->
                    "function definition"

                Let ->
                    "init local variable"

                Set ->
                    "set local variable"

                Get ->
                    "get local variable"

                Br ->
                    "break"

                BrIf ->
                    "break if"

                Return ->
                    "return"

                Call ->
                    "call function"

                Nop ->
                    "no operation"

                Drop ->
                    "drop"

                Malloc ->
                    "allocate n bytes of memory"

                Read1 ->
                    "read 1 byte"

                Read2 ->
                    "read 2 byte"

                Read4 ->
                    "read 4 byte"

                Write1 ->
                    "write 1 byte"

                Write2 ->
                    "write 2 byte"

                Write4 ->
                    "write 4 byte"


instructions : List Instruction
instructions =
    [ Op Add
    , Op Sub
    , Op Mul
    , Op Div
    , Op Rem
    , Op Gt
    , Op Gte
    , Op Lt
    , Op Lte
    , Op Eq
    , Op Neq
    , Op And
    , Op Or
    , Op Xor
    , Op Rsh
    , Op Lsh
    , Op Fun
    , Op Num
    , Op Let
    , Op Set
    , Op Get
    , CF Block
    , CF Loop
    , CF If
    , CF Else
    , Op Br
    , Op BrIf
    , Op Return
    , Op Call
    , Op Nop
    , Op Drop
    , Op Malloc
    , Op Read1
    , Op Read2
    , Op Read4
    , Op Write1
    , Op Write2
    , Op Write4
    ]


toHtml : List Instruction -> List (Html Msg)
toHtml ins =
    List.map (\instr -> div [ class "op", draggable "true" ] [ text (show instr) ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
