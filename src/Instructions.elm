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
    | Let Variable
    | Set Variable
    | Get Variable
    | Fun Variable Arity Coarity
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


getMeta : Instr -> { button : String, docs : Docs, signature : Signature, class : String }
getMeta instr =
    case instr of
        EmptyLine ->
            { button = "~", docs = "empty line", signature = Sig 0 0, class = "empty" }

        Fun _ a ca ->
            { button = "fun", docs = "function definition", signature = Sig a ca, class = "function" }

        Block _ ca ->
            { button = "block", docs = "block $block_label +coarity. breaking to block label jumps out of block.", signature = Sig 0 ca, class = "control-flow" }

        Loop _ ca ->
            { button = "loop", docs = "breaking to loop label jumps to top of loop", signature = Sig 0 ca, class = "control-flow" }

        If _ ca ->
            { button = "if", docs = "breaking to if label jumps out of if", signature = Sig 1 ca, class = "control-flow" }

        End ->
            { button = "end", docs = "end of function, block, loop, or if", signature = Sig 0 0, class = "control-flow" }

        Else ->
            { button = "else", docs = "else branch of if statement", signature = Sig 0 0, class = "control-flow" }

        Add ->
            { button = "+", docs = "addition", signature = Sig 2 1, class = "numeric" }

        Sub ->
            { button = "sub", docs = "subtraction", signature = Sig 2 1, class = "numeric" }

        Mul ->
            { button = "mult", docs = "multiplication", signature = Sig 2 1, class = "numeric" }

        Div ->
            { button = "div", docs = "division", signature = Sig 2 1, class = "numeric" }

        Rem ->
            { button = "%", docs = "remainder", signature = Sig 2 1, class = "numeric" }

        Gt ->
            { button = ">", docs = "greater than", signature = Sig 2 1, class = "numeric" }

        Gte ->
            { button = "≥", docs = "greater than or equal", signature = Sig 2 1, class = "numeric" }

        Lt ->
            { button = "<", docs = "less than", signature = Sig 2 1, class = "numeric" }

        Lte ->
            { button = "≤", docs = "less than or equal", signature = Sig 2 1, class = "numeric" }

        Eq ->
            { button = "=", docs = "equal", signature = Sig 2 1, class = "numeric" }

        Neq ->
            { button = "≠", docs = "not equal", signature = Sig 2 1, class = "numeric" }

        And ->
            { button = "and", docs = "bitwise/logical and", signature = Sig 2 1, class = "numeric" }

        Or ->
            { button = "or", docs = "bitwise/logical or", signature = Sig 2 1, class = "numeric" }

        Xor ->
            { button = "xor", docs = "bitwise/logical xor", signature = Sig 2 1, class = "numeric" }

        Rsh ->
            { button = "»", docs = "bitwise right shift", signature = Sig 2 1, class = "numeric" }

        Lsh ->
            { button = "«", docs = "bitwise left shift", signature = Sig 2 1, class = "numeric" }

        Num _ ->
            { button = "num", docs = "constant integer", signature = Sig 0 1, class = "numeric" }

        Let _ ->
            { button = "let", docs = "local variable declaration", signature = Sig 0 0, class = "variable" }

        Set _ ->
            { button = "set", docs = "set local variable", signature = Sig 1 0, class = "variable" }

        Get _ ->
            { button = "get", docs = "get local variable", signature = Sig 0 1, class = "variable" }

        Br _ ->
            { button = "br", docs = "break to label", signature = Sig 1 0, class = "control-flow" }

        BrIf _ ->
            { button = "br_if", docs = "if truthy break to label", signature = Sig 1 0, class = "control-flow" }

        Call _ ->
            { button = "call", docs = "call function", signature = Sig -1 -1, class = "control-flow" }

        Return ->
            { button = "return", docs = "return function", signature = Sig 0 0, class = "control-flow" }

        Nop ->
            { button = "nop", docs = "no operation", signature = Sig 0 0, class = "control-flow" }

        Drop ->
            { button = "drop", docs = "drop top of stack", signature = Sig 1 0, class = "control-flow" }

        Malloc ->
            { button = "malloc", docs = "allocate n bytes", signature = Sig 1 1, class = "memory" }

        Free ->
            { button = "free", docs = "free memory allocated with malloc", signature = Sig 1 0, class = "memory" }

        Read1 ->
            { button = "read1", docs = "read 1 bytee", signature = Sig 1 1, class = "memory" }

        Read2 ->
            { button = "read2", docs = "read 2 bytes", signature = Sig 1 1, class = "memory" }

        Read4 ->
            { button = "read4", docs = "read 4 bytes", signature = Sig 1 1, class = "memory" }

        Write1 ->
            { button = "write1", docs = "write 1 byte", signature = Sig 2 0, class = "memory" }

        Write2 ->
            { button = "write2", docs = "write 2 bytes", signature = Sig 2 0, class = "memory" }

        Write4 ->
            { button = "write4", docs = "write 4 bytes", signature = Sig 2 0, class = "memory" }


instructions : List Instr
instructions =
    [ Add
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
    , Fun "my_fun" 0 0
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
