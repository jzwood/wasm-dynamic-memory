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
    = Operator Op
    | ControlFlow CF


type CF
    = Block
    | Loop
    | If
    | Else


type Op
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
        ControlFlow cf ->
            case cf of
                Block ->
                    "block"

                Loop ->
                    "loop"

                If ->
                    "if"

                Else ->
                    "else"

        Operator op ->
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
                    "num"

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
        ControlFlow cf ->
            case cf of
                Block ->
                    "block"

                Loop ->
                    "loop"

                If ->
                    "if"

                Else ->
                    "else"

        Operator op ->
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
                    "num"

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


instructions : List Instruction
instructions =
    []


toHtml : List Instruction -> List (Html Msg)
toHtml ins =
    List.map (\instr -> div [ class "op", draggable "true" ] [ text (show instr) ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
