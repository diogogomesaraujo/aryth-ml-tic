open Interpreter

let parse (s : string) : Ast.expr option =
  try
    let lexbuf = Lexing.from_string s in
    try
      Some(Parser.prog Lexer.read lexbuf)
      with
      | Parsing.Parse_error ->
        prerr_endline "Syntax error!";
        None
  with
  | _ ->
    prerr_endline "Lexing Error!";
    None

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

let rec main () =
  print_endline "Escreva uma expressão para números reais ( X. ): ";
  let i = read_line () in
  match parse i with
  | Some parsed ->
    let result = calc parsed in
    "Resultado: " ^ string_of_float result |> print_endline;
    main ()
  | None ->
    main ()

let () = print_endline ""; main ()
