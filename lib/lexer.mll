{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ ('.' digit+?)

rule read =
    parse
    | white { read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
    | "=" { EQUALS }
    | "," { COMMA }
    | "(" { LPAR }
    | ")" { RPAR }
    | "+" { SUM }
    | "-" { SUB }
    | "*" { MUL }
    | "/" { DIV }
    | "^" { POW }
    | "root" { ROOT }
    | "log" { LOG }
    | "let" { LET }
    | ['a'-'z' 'A'-'Z' '_']+ { ID (Lexing.lexeme lexbuf) }
    | eof { EOF }
