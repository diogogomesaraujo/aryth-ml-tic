open Interpreter

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec calc e =
  match e with
  | Ast.Int i -> i
  | Ast.Bop (b, e1, e2) ->
    match b with
    | Ast.Sum -> (calc e1) + (calc e2)
    | Ast.Sub -> (calc e1) - (calc e2)
    | Ast.Mul -> (calc e1) * (calc e2)
    | Ast.Div -> (calc e1) / (calc e2)

let () =
  print_endline "";
  print_endline "Escreva uma expressão para números inteiros: ";
  let result = read_line () |> parse |> calc in
  string_of_int result |> print_endline
