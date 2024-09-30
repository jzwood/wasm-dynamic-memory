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
    List.repeat ca "(result i32)" |> String.join "\n "


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
            { button = "block", docs = "block $label: pop 0, push ^coarity. breaking to label jumps out of block.", class = "control-flow", wat = String.concat [ "block ", "$", label, "\n ", coarityToWat ca ] }

        Loop label ca ->
            { button = "loop", docs = "breaking to loop label jumps to top of loop", class = "control-flow", wat = String.concat [ "loop ", "$", label, "\n ", coarityToWat ca ] }

        If label ca ->
            { button = "if", docs = "breaking to if label jumps out of if", class = "control-flow", wat = String.concat [ "if ", "$", label, "\n ", coarityToWat ca ] }

        End ->
            { button = "end", docs = "end of function, block, loop, or if", class = "control-flow", wat = "end" }

        Else ->
            { button = "else", docs = "else branch of if statement", class = "control-flow", wat = "else" }

        Add ->
            { button = "+", docs = "addition", class = "numeric", wat = "i32.add" }

        Sub ->
            { button = "sub", docs = "subtraction", class = "numeric", wat = "i32.sub" }

        Mul ->
            { button = "mult", docs = "multiplication", class = "numeric", wat = "i32.mul" }

        Div ->
            { button = "div", docs = "division", class = "numeric", wat = "i32.div_s" }

        Rem ->
            { button = "%", docs = "remainder", class = "numeric", wat = "i32.rem" }

        Gt ->
            { button = ">", docs = "greater than", class = "numeric", wat = "i32.gt" }

        Gte ->
            { button = "≥", docs = "greater than or equal", class = "numeric", wat = "i32.ge" }

        Lt ->
            { button = "<", docs = "less than", class = "numeric", wat = "i32.lt" }

        Lte ->
            { button = "≤", docs = "less than or equal", class = "numeric", wat = "i32.le" }

        Eq ->
            { button = "=", docs = "equal", class = "numeric", wat = "i32.eq" }

        Neq ->
            { button = "≠", docs = "not equal", class = "numeric", wat = "i32.neq" }

        And ->
            { button = "and", docs = "bitwise/logical and", class = "numeric", wat = "i32.and" }

        Or ->
            { button = "or", docs = "bitwise/logical or", class = "numeric", wat = "i32.or" }

        Xor ->
            { button = "xor", docs = "bitwise/logical xor", class = "numeric", wat = "i32.xor" }

        Rsh ->
            { button = "»", docs = "bitwise right shift", class = "numeric", wat = "i32.shr_s" }

        Lsh ->
            { button = "«", docs = "bitwise left shift", class = "numeric", wat = "i32.shl" }

        Num n ->
            { button = "num", docs = "constant integer", class = "numeric", wat = unwords [ "i32.const", String.fromInt n ] }

        Arg a ->
            { button = "arg", docs = "function argument", class = "variable", wat = String.concat [ "(param $", a, " i32)" ] }

        Let label ->
            { button = "let", docs = "local variable declaration", class = "variable", wat = String.concat [ "(local $", label, " i32)" ] }

        Set _ ->
            { button = "set", docs = "set local variable", class = "variable", wat = "locat.set" }

        Get _ ->
            { button = "get", docs = "get local variable", class = "variable", wat = "local.get" }

        Br _ ->
            { button = "br", docs = "break to label", class = "control-flow", wat = "br" }

        BrIf _ ->
            { button = "br_if", docs = "if truthy break to label", class = "control-flow", wat = "br_if" }

        Call fxn ->
            { button = "call", docs = "call function", class = "control-flow", wat = "call $" ++ fxn }

        Return ->
            { button = "return", docs = "return function", class = "control-flow", wat = "return" }

        Nop ->
            { button = "nop", docs = "no operation", class = "control-flow", wat = "nop" }

        Drop ->
            { button = "drop", docs = "drop top of stack", class = "control-flow", wat = "drop" }

        Malloc ->
            { button = "malloc", docs = "allocate n bytes", class = "memory", wat = "call $malloc" }

        Free ->
            { button = "free", docs = "free memory allocated with malloc", class = "memory", wat = "call $free" }

        Read1 ->
            { button = "read1", docs = "read 1 bytee", class = "memory", wat = "i32.load8_s" }

        Read2 ->
            { button = "read2", docs = "read 2 bytes", class = "memory", wat = "i32.load16_s" }

        Read4 ->
            { button = "read4", docs = "read 4 bytes", class = "memory", wat = "i32.load" }

        Write1 ->
            { button = "write1", docs = "write 1 byte", class = "memory", wat = "i32.store8" }

        Write2 ->
            { button = "write2", docs = "write 2 bytes", class = "memory", wat = "i32.store16" }

        Write4 ->
            { button = "write4", docs = "write 4 bytes", class = "memory", wat = "i32.store" }


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
