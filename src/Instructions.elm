module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)


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
