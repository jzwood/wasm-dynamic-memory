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
    | Float
    | Bool


type Category
    = Numeric
    | Variadic
    | Memory
    | ControlFlow


type Label
    = None
    | Explicit
    | Inferred


type Instruction
    = Instruction { id : InstructionType, desc : String, show : String, category : Category, signature : Signature }


type Signature
    = Signature { params : List Type, results : List Type }



--- Abstract Syntax Tree


type AST
    = Module
        { globals : List Global
        , functions : List Function
        }


type Global
    = Global


type Function
    = Function
        { name : String
        , signature : Signature
        , body : List Instruction
        }


type InstructionType
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
    | Set
    | Get
    | Block
    | If
    | Loop
    | Malloc
    | Read
    | Write


instructions : List Instruction
instructions =
    [ Instruction { id = Add, desc = "add", show = "+", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Sub, desc = "subtract", show = "-", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Mul, desc = "multiply", show = "*", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Div, desc = "divide", show = "/", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Rem, desc = "remainder", show = "%", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Gt, desc = "greater than", show = ">", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = Gte, desc = "greater than or equal to", show = "≥", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = Lt, desc = "less than", show = "<", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = Lte, desc = "less than or equal to", show = "≥", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = Eq, desc = "equal", show = "=", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = Neq, desc = "not equal", show = "≠", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { id = And, desc = "bitwise/logical and", show = "and", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Or, desc = "bitwise/logical or", show = "or", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Xor, desc = "bitwise/logical xor", show = "xor", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Lsh, desc = "left shift", show = "«", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { id = Rsh, desc = "right shift", show = "»", category = Numeric, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }

    -- flow
    , Instruction { id = Num, desc = "constant number", show = "num", category = Numeric, signature = Signature { params = [ Integer ], results = [] } }
    , Instruction { id = Fun, desc = "function definition", show = "f(x)", category = Numeric, signature = Signature { params = [], results = [] } }
    , Instruction { id = Set, desc = "set local variable", show = "set", category = Variadic, signature = Signature { params = [ Integer ], results = [] } }
    , Instruction { id = Get, desc = "get local variable", show = "get", category = Variadic, signature = Signature { params = [ Integer ], results = [] } }

    -- control
    , Instruction { id = Block, desc = "block", show = "block", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { id = If, desc = "if statement", show = "if", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { id = Loop, desc = "loop", show = "loop", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { id = Malloc, desc = "allocate n bytes of memory", show = "malloc", category = Memory, signature = Signature { params = [ Integer ], results = [ Pointer ] } }
    , Instruction { id = Read, desc = "read 1 byte", show = "read1", category = Memory, signature = Signature { params = [ Pointer ], results = [ Integer ] } }
    , Instruction { id = Read, desc = "read 2 byte", show = "read2", category = Memory, signature = Signature { params = [ Pointer ], results = [ Integer ] } }
    , Instruction { id = Read, desc = "read 4 byte", show = "read4", category = Memory, signature = Signature { params = [ Pointer ], results = [ Integer ] } }
    , Instruction { id = Write, desc = "write 1 byte", show = "write1", category = Memory, signature = Signature { params = [ Pointer, Integer ], results = [] } }
    , Instruction { id = Write, desc = "write 2 byte", show = "write2", category = Memory, signature = Signature { params = [ Pointer, Integer ], results = [] } }
    , Instruction { id = Write, desc = "write 4 byte", show = "write4", category = Memory, signature = Signature { params = [ Pointer, Integer ], results = [] } }
    ]


toHtml : List Instruction -> List (Html Msg)
toHtml ins =
    List.map (\(Instruction { show, category, signature }) -> div [ class "op", draggable "true" ] [ text show ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
