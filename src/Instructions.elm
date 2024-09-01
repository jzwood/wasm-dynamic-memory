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
    = Signature (List Type) Result



--- Abstract Syntax Tree


type AST
    = Module (List Function)


type Function
    = Function Label Signature Children


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
    = UI { instr : Instruction, show : String, docs : Docs }



--type Meta
--= Instr Instruction Label Docs Signature
--| Fun Docs Signature


instructions : List UI
instructions =
    [ UI { instr = Add, show = "+", docs = "addition" } ]


toHtml : List UI -> List (Html Msg)
toHtml ins =
    List.map (\(UI ui) -> div [ class "op", draggable "true" ] [ text ui.show ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
