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
    | Bool


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



--- Abstract Syntax Tree


type AST
    = Module Children


type Instruction
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


type UI
    = UI { instr : Instruction, button : String, docs : Docs }



--type Meta
--= Instr Instruction Label Docs Signature
--| Fun Docs Signature


instructions : List UI
instructions =
    [ UI { instr = Add, button = "+", docs = "addition" }
    , UI { instr = Block "" [] [], button = "block", docs = "breaking to block label jumps out of block" }
    , UI { instr = Loop "" [] [], button = "loop", docs = "breaking to loop label jumps to top of loop" }
    , UI { instr = If "" [] [] [], button = "if", docs = "breaking to if label jumps out of if" }
    , UI { instr = Else [], button = "else", docs = "else branch of if statement" }
    , UI { instr = Add, button = "+", docs = "addition" }
    , UI { instr = Sub, button = "sub", docs = "subtraction" }
    , UI { instr = Mul, button = "mult", docs = "multiplication" }
    , UI { instr = Div, button = "div", docs = "division" }
    , UI { instr = Rem, button = "%", docs = "remainder" }
    , UI { instr = Gt, button = ">", docs = "greater than" }
    , UI { instr = Gte, button = "≥", docs = "greater than or equal" }
    , UI { instr = Lt, button = "<", docs = "less than" }
    , UI { instr = Lte, button = "≤", docs = "less than or equal" }
    , UI { instr = Eq, button = "=", docs = "equal" }
    , UI { instr = Neq, button = "≠", docs = "not equal" }
    , UI { instr = And, button = "and", docs = "bitwise/logical and" }
    , UI { instr = Or, button = "or", docs = "bitwise/logical or" }
    , UI { instr = Xor, button = "xor", docs = "bitwise/logical xor" }
    , UI { instr = Rsh, button = "»", docs = "bitwise right shift" }
    , UI { instr = Lsh, button = "«", docs = "bitwise left shift" }
    , UI { instr = Num 0, button = "num", docs = "constant number" }
    , UI { instr = Fun "" (Sig [] []) [], button = "f(x)", docs = "function definition" }
    , UI { instr = Let "", button = "let", docs = "local variable declaration" }
    , UI { instr = Set "", button = "set", docs = "set local variable" }
    , UI { instr = Get "", button = "get", docs = "get local variable" }
    , UI { instr = Br "", button = "br", docs = "break to label" }
    , UI { instr = BrIf "", button = "br_if", docs = "if truthy break to label" }
    , UI { instr = Return, button = "return", docs = "return function" }
    , UI { instr = Call "", button = "call", docs = "call function" }
    , UI { instr = Nop, button = "nop", docs = "no operation" }
    , UI { instr = Drop, button = "drop", docs = "drop top of stack" }
    , UI { instr = Malloc, button = "malloc", docs = "allocate n bytes" }
    , UI { instr = Read1, button = "read1", docs = "read 1 bytee" }
    , UI { instr = Read2, button = "read2", docs = "read 2 bytes" }
    , UI { instr = Read4, button = "read4", docs = "read 4 bytes" }
    , UI { instr = Write1, button = "write1", docs = "write 1 byte" }
    , UI { instr = Write2, button = "write2", docs = "write 2 bytes" }
    , UI { instr = Write4, button = "write4", docs = "write 4 bytes" }
    ]


toHtml : List UI -> List (Html Msg)
toHtml ins =
    List.map (\(UI ui) -> div [ class "op", draggable "true" ] [ text ui.button ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
