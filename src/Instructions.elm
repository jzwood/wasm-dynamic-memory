module Instructions exposing (..)

import Html exposing (Html, aside, button, div, math, section, span, text)
import Html.Attributes exposing (attribute, class, draggable, id, style)
import List.Extra exposing (groupWhile)


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


getMeta : Instr -> { button : String, docs : Docs, class : String }
getMeta instr =
    case instr of
        DEL ->
            { button = "DEL", docs = "delete line below", class = "delete" }

        EmptyLine ->
            { button = "~", docs = "empty line", class = "empty" }

        Fun _ ca ->
            { button = "fun", docs = "function definition", class = "function" }

        Block _ ca ->
            { button = "block", docs = "block $block_label +coarity. breaking to block label jumps out of block.", class = "control-flow" }

        Loop _ ca ->
            { button = "loop", docs = "breaking to loop label jumps to top of loop", class = "control-flow" }

        If _ ca ->
            { button = "if", docs = "breaking to if label jumps out of if", class = "control-flow" }

        End ->
            { button = "end", docs = "end of function, block, loop, or if", class = "control-flow" }

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
            { button = "num", docs = "constant integer", class = "numeric" }

        Arg _ ->
            { button = "arg", docs = "function argument", class = "variable" }

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

        Call _ ->
            { button = "call", docs = "call function", class = "control-flow" }

        Return ->
            { button = "return", docs = "return function", class = "control-flow" }

        Nop ->
            { button = "nop", docs = "no operation", class = "control-flow" }

        Drop ->
            { button = "drop", docs = "drop top of stack", class = "control-flow" }

        Malloc ->
            { button = "malloc", docs = "allocate n bytes", class = "memory" }

        Free ->
            { button = "free", docs = "free memory allocated with malloc", class = "memory" }

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
