type bop =
  | Sum
  | Sub
  | Mul
  | Div
  | Pow
  | Root
  | Log
[@@deriving show]

type expr =
  | Float of float
  | Int of int
  | Bop of bop * expr * expr
[@@deriving show]
