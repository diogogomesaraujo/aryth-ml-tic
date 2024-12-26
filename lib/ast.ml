type bop =
  | Sum
  | Sub
  | Mul
  | Div
  | Pow
[@@deriving show]

type expr =
  | Int of int
  | Bop of bop * expr * expr
[@@deriving show]
