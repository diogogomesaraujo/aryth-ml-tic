open Interpreter
module StringMap = Map.Make(String)

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

let eval e map =
  match e with
  | Ast.Let (x, e) -> (e, (StringMap.add x e map))
  | Ast.Var v -> (StringMap.find v map, map)
  | Ast.Float f -> (Ast.Float f, map)
  | Ast.Int i -> (Ast.Int i, map)
  | Ast.Bop (b, e1, e2)-> (Ast.Bop(b, e1, e2), map)

let rec calc e map =
  match e with
  | Ast.Float f -> f
  | Ast.Int i -> float_of_int i
  | Ast.Var v -> calc (StringMap.find v map) map
  | Ast.Let (_, _) -> prerr_endline "error with the expression"; exit 1
  | Ast.Bop (b, e1, e2) ->
    match b with
    | Ast.Sum -> (calc e1 map) +. (calc e2 map)
    | Ast.Sub -> (calc e1 map) -. (calc e2 map)
    | Ast.Mul -> (calc e1 map) *. (calc e2 map)
    | Ast.Div -> (calc e1 map) /. (calc e2 map)
    | Ast.Pow -> Float.pow (calc e1 map) (calc e2 map)
    | Ast.Root -> Float.pow (calc e1 map) (1. /. (calc e2 map))
    | Ast.Log -> Float.log2 (calc e2 map) /. Float.log2 (calc e1 map)
    | _ -> prerr_endline "not matched yet"; exit 1

let rec main map () =
  print_endline "Escreva uma expressão para números reais: ";
  let i = read_line () in
  match parse i with
  | Some parsed ->
    let (evaled, map) = eval parsed map in
    let result = calc evaled map in
    "Resultado: " ^ string_of_float result |> print_endline;
    StringMap.iter (fun k _ -> Printf.printf "Key: %s\n" k ) map;
    main map ()
  | None ->
    main map ()

let () = print_endline ""; main StringMap.empty ()
