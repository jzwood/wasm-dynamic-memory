module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)
import Utils exposing (..)


type alias Cursor =
    Int


type alias Arity =
    Int


type alias Coarity =
    Int


type Signature
    = Sig Arity Coarity


type alias Label =
    String


type alias Docs =
    String


type alias Variable =
    String


type alias Children =
    List Instr


type Instr
    = EmptyLine
    | DEL
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
    | Arg Variable
    | Let Variable
    | Set Variable
    | Get Variable
    | Fun Variable Coarity
    | Block Label Coarity
    | Loop Label Coarity
    | If Label Coarity
    | Else
    | End
    | Br Label
    | BrIf Label
    | Return
    | Call Variable
    | Nop
    | Drop
    | Malloc
    | Free
    | Read1
    | Read2
    | Read4
    | Write1
    | Write2
    | Write4


coarityToWat : Int -> String
coarityToWat ca =
    ""


getMeta : Instr -> { button : String, docs : Docs, class : String, wat : String }
getMeta instr =
    case instr of
        DEL ->
            { button = "DEL", docs = "delete line", class = "delete", wat = "" }

        EmptyLine ->
            { button = "~", docs = "empty line", class = "empty", wat = "" }

        Fun _ ca ->
            { button = "fun", docs = "function definition", class = "function", wat = "(func" }

        Block label ca ->
            { button = "block", docs = "block $label: pop 0, push ^coarity. breaking to label jumps out of block.", class = "control-flow", wat = unwords [ "block", "$" ++ label, coarityToWat ca ] }

        Loop _ ca ->
            { button = "loop", docs = "breaking to loop label jumps to top of loop", class = "control-flow", wat = "" }

        If _ ca ->
            { button = "if", docs = "breaking to if label jumps out of if", class = "control-flow", wat = "" }

        End ->
            { button = "end", docs = "end of function, block, loop, or if", class = "control-flow", wat = "" }

        Else ->
            { button = "else", docs = "else branch of if statement", class = "control-flow", wat = "" }

        Add ->
            { button = "+", docs = "addition", class = "numeric", wat = "" }

        Sub ->
            { button = "sub", docs = "subtraction", class = "numeric", wat = "" }

        Mul ->
            { button = "mult", docs = "multiplication", class = "numeric", wat = "" }

        Div ->
            { button = "div", docs = "division", class = "numeric", wat = "" }

        Rem ->
            { button = "%", docs = "remainder", class = "numeric", wat = "" }

        Gt ->
            { button = ">", docs = "greater than", class = "numeric", wat = "" }

        Gte ->
            { button = "≥", docs = "greater than or equal", class = "numeric", wat = "" }

        Lt ->
            { button = "<", docs = "less than", class = "numeric", wat = "" }

        Lte ->
            { button = "≤", docs = "less than or equal", class = "numeric", wat = "" }

        Eq ->
            { button = "=", docs = "equal", class = "numeric", wat = "" }

        Neq ->
            { button = "≠", docs = "not equal", class = "numeric", wat = "" }

        And ->
            { button = "and", docs = "bitwise/logical and", class = "numeric", wat = "" }

        Or ->
            { button = "or", docs = "bitwise/logical or", class = "numeric", wat = "" }

        Xor ->
            { button = "xor", docs = "bitwise/logical xor", class = "numeric", wat = "" }

        Rsh ->
            { button = "»", docs = "bitwise right shift", class = "numeric", wat = "" }

        Lsh ->
            { button = "«", docs = "bitwise left shift", class = "numeric", wat = "" }

        Num _ ->
            { button = "num", docs = "constant integer", class = "numeric", wat = "" }

        Arg _ ->
            { button = "arg", docs = "function argument", class = "variable", wat = "" }

        Let _ ->
            { button = "let", docs = "local variable declaration", class = "variable", wat = "" }

        Set _ ->
            { button = "set", docs = "set local variable", class = "variable", wat = "" }

        Get _ ->
            { button = "get", docs = "get local variable", class = "variable", wat = "" }

        Br _ ->
            { button = "br", docs = "break to label", class = "control-flow", wat = "" }

        BrIf _ ->
            { button = "br_if", docs = "if truthy break to label", class = "control-flow", wat = "" }

        Call _ ->
            { button = "call", docs = "call function", class = "control-flow", wat = "" }

        Return ->
            { button = "return", docs = "return function", class = "control-flow", wat = "" }

        Nop ->
            { button = "nop", docs = "no operation", class = "control-flow", wat = "" }

        Drop ->
            { button = "drop", docs = "drop top of stack", class = "control-flow", wat = "" }

        Malloc ->
            { button = "malloc", docs = "allocate n bytes", class = "memory", wat = "" }

        Free ->
            { button = "free", docs = "free memory allocated with malloc", class = "memory", wat = "" }

        Read1 ->
            { button = "read1", docs = "read 1 bytee", class = "memory", wat = "" }

        Read2 ->
            { button = "read2", docs = "read 2 bytes", class = "memory", wat = "" }

        Read4 ->
            { button = "read4", docs = "read 4 bytes", class = "memory", wat = "" }

        Write1 ->
            { button = "write1", docs = "write 1 byte", class = "memory", wat = "" }

        Write2 ->
            { button = "write2", docs = "write 2 bytes", class = "memory", wat = "" }

        Write4 ->
            { button = "write4", docs = "write 4 bytes", class = "memory", wat = "" }


instructions : List Instr
instructions =
    [ DEL
    , EmptyLine
    , Add
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
    , Fun "my_fun" 0
    , Arg "arg"
    , Let "var"
    , Set "var"
    , Get "var"
    , Block "bl" 0
    , Loop "lo" 0
    , If "if" 0
    , Else
    , End
    , Br "lo"
    , BrIf "lo"
    , Return
    , Call "my_fun"
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
