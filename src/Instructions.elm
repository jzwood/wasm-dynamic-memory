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

type alias Result = List Type
type alias Label = String
type alias Variable = String
type alias Children = List Instruction

type Signature = Signature { params : List Type, result : Result }

--- Abstract Syntax Tree
type AST = Module (List Function)

type Function = Function Label Signature Children

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
    | Call Label
    | Nop
    | Drop
    | Malloc
    | Read1
    | Read2
    | Read4
    | Write1
    | Write2
    | Write4

type Meta
    = AddMeta
    | SubMeta
    | MulMeta
    | DivMeta
    | RemMeta
    | GtMeta
    | GteMeta
    | LtMeta
    | LteMeta
    | EqMeta
    | NeqMeta
    | AndMeta
    | OrMeta
    | XorMeta
    | RshMeta
    | LshMeta
    | NumMeta
    | LetMeta
    | SetMeta
    | GetMeta
    | BlockMeta
    | LoopMeta
    | IfMeta
    | ElseMeta
    | BrMeta
    | BrIfMeta
    | ReturnMeta
    | CallMeta
    | NopMeta
    | DropMeta
    | MallocMeta
    | Read1Meta
    | Read2Meta
    | Read4Meta
    | Write1Meta
    | Write2Meta
    | Write4Meta

instructions : List Instruction
instructions =
    []


toHtml : List Instruction -> List (Html Msg)
toHtml ins =
    List.map (\instr -> div [ class "op", draggable "true" ] [ text (show instr) ]) ins



--ins
--|> groupWhile (\(Instruction a) (Instruction b) -> a.category == b.category)
--|> List.concatMap
--(\( i0, is ) -> List.map (\(Instruction i) -> div [ class "op", draggable "true" ] [ text i.show ]) (i0 :: is))
