module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)


type Msg
    = Increment
    | Decrement


type Type
    = Integer
    | Float
    | Bool


type Instruction
    = Instruction { label : String, op : String, category : Category, signature : Signature }


type Category
    = Math
    | ControlFlow
    | Variadic
    | Memory
    | UserDefined


type Signature
    = Signature { params : List Type, results : List Type }


instructions : List Instruction
instructions =
    [ Instruction { label = "plus", op = "+", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "minus", op = "-", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "multiply", op = "*", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "divide", op = "/", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "remainder", op = "%", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "greater than", op = ">", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = "greater than or equal to", op = "≥", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = "less thanb", op = "<", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = " less than or equal to", op = "≥", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = "equal", op = "=", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = "not equal", op = "≠", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Instruction { label = "bitwise/logical and", op = "and", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "bitwise/logical or", op = "or", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "bitwise/logical xor", op = "xor", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "left shift", op = "«", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Instruction { label = "right shift", op = "»", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }

    -- flow
    , Instruction { label = "function", op = "fun", category = Math, signature = Signature { params = [], results = [] } }
    , Instruction { label = "set", op = "set", category = Variadic, signature = Signature { params = [ Integer ], results = [] } }
    , Instruction { label = "get", op = "get", category = Variadic, signature = Signature { params = [ Integer ], results = [] } }

    -- control
    , Instruction { label = "block", op = "block", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { label = "if statement", op = "if", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { label = "loop", op = "loop", category = ControlFlow, signature = Signature { params = [], results = [] } }
    , Instruction { label = "allocate n bytes of memory", op = "malloc", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "read 1 byte", op = "read1", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "read 2 byte", op = "read2", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "read 4 byte", op = "read4", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "write 1 byte", op = "write1", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "write 2 byte", op = "write2", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    , Instruction { label = "write 4 byte", op = "write4", category = Memory, signature = Signature { params = [ Integer ], results = [ Integer ] } }
    ]


toHtml : List Instruction -> List (Html Msg)
toHtml ins =
    List.map (\(Instruction { op, category, signature }) -> div [ class "op", draggable "true" ] [ text op ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.op ]) (i0 :: is))
