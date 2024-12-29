type bop =
  | Let
  | Sum
  | Sub
  | Mul
  | Div
  | Pow
  | Root
  | Log
[@@deriving show]

type expr =
  | Var of string
  | Let of string * expr
  | Float of float
  | Int of int
  | Bop of bop * expr * expr
[@@deriving show]
