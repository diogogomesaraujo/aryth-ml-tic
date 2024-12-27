open Interpreter

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec calc e =
  match e with
  | Ast.Float f -> f
  | Ast.Bop (b, e1, e2) ->
    match b with
    | Ast.Sum -> (calc e1) +. (calc e2)
    | Ast.Sub -> (calc e1) -. (calc e2)
    | Ast.Mul -> (calc e1) *. (calc e2)
    | Ast.Div -> (calc e1) /. (calc e2)
    | Ast.Pow -> Float.pow (calc e1) (calc e2)
    | Ast.Root -> Float.pow (calc e1) (1. /. (calc e2))

let () =
  print_endline "";
  print_endline "Escreva uma expressão para números reais ( X.X ): ";
  let result = read_line () |> parse |> calc in
  "Resultado: " ^ string_of_float result |> print_endline
