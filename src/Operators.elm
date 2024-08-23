module Operators exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (class, draggable, id, style)


type Msg
    = Increment
    | Decrement


type Type
    = Integer
    | Float
    | Bool


type Operator
    = Operator { label : String, op : String, category : Category, signature : Signature }


type Category
    = Math
    | ControlFlow
    | UserDefined


type Signature
    = Signature { params : List Type, results : List Type }


operators : List Operator
operators =
    [ Operator { label = "plus", op = "+", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "minus", op = "-", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "multiply", op = "*", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "divide", op = "/", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "remainder", op = "%", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "greater than", op = ">", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = "greater than or equal to", op = "≥", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = "less thanb", op = "<", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = " less than or equal to", op = "≥", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = "equal", op = "=", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = "not equal", op = "≠", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Bool ] } }
    , Operator { label = "bitwise or", op = "|", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "bitwise and", op = "&", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "bitwise xor", op = "^", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "left shift", op = "«", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "right shift", op = "»", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "floot", op = "⎿", category = Math, signature = Signature { params = [ Float, Float ], results = [ Float ] } }
    , Operator { label = "ceiling", op = "⎾", category = Math, signature = Signature { params = [ Float, Float ], results = [ Float ] } }
    , Operator { label = "absolute value", op = "[]", category = Math, signature = Signature { params = [ Integer, Integer ], results = [ Integer ] } }
    , Operator { label = "logical not", op = "¬", category = Math, signature = Signature { params = [ Bool ], results = [ Bool ] } }
    , Operator { label = "logical or", op = "∨", category = Math, signature = Signature { params = [ Bool, Bool ], results = [ Bool ] } }
    , Operator { label = "logical and", op = "∧", category = Math, signature = Signature { params = [ Bool, Bool ], results = [ Bool ] } }
    , Operator { label = "logical xor", op = "⊻", category = Math, signature = Signature { params = [ Bool, Bool ], results = [ Bool ] } }

    -- TODO
    , Operator { label = "float to int", op = "i", category = Math, signature = Signature { params = [ Float ], results = [ Integer ] } }
    , Operator { label = "int to float", op = "f", category = Math, signature = Signature { params = [ Integer ], results = [ Float ] } }
    , Operator { label = "constant integer", op = "i", category = Math, signature = Signature { params = [], results = [ Integer ] } } -- TODO fix up
    ]


showOperator : Operator -> Html Msg
showOperator (Operator { op, category, signature }) =
    math [ class "op", draggable "true" ] [ text op ]
