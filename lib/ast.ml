type bop =
  | Sum
  | Sub
  | Mul
  | Div
  | Pow
  | Root
[@@deriving show]

type expr =
  | Float of float
  | Bop of bop * expr * expr
[@@deriving show]
