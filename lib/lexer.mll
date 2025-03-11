{
  open Lexing
  open Parser
}

let spaces = [' ' '\t' '\n' '\r']+
let digits = ['0'-'9']
let lowercase_chars = ['a'-'z']
let uppercase_chars = ['A'-'Z']
let chars = lowercase_chars | uppercase_chars
let int   = '-'? digits+
let ident = (chars | '_')(chars | digits | '_')* '\''*

rule tokenize = parse
  | spaces  { tokenize lexbuf }
  | "(*"    { comment lexbuf; tokenize lexbuf }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '!'     { NOT }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MUL }
  | '/'     { DIV }
  | '='     { EQ }
  | "||"    { LOR }
  | "&&"    { LAND }
  | "<>"    { NEQ }
  | '<'     { LT }
  | "<="    { LEQ }
  | '>'     { GT }
  | ">="    { GEQ }
  | "let"   { LET }
  | "rec"   { REC }
  | "in"    { IN }
  | "fun"   { FUN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "->"    { ARROW }
  | ";"     { SEMICOLON }
  | "_"     { IDENT (Id.gen_tmp Type.Unit) }
  | "true"  { BOOL (true) }
  | "false" { BOOL (false) }
  | int     { INT (int_of_string (lexeme lexbuf)) }
  | ident   { IDENT (lexeme lexbuf) }
  | eof     { EOF }
  | _ as c  { failwith (Printf.sprintf "Unexpected character: %c" c) }

and comment = parse
  | "*)"  { () }
  | "(*"  { comment lexbuf; comment lexbuf }
  | eof   { Format.eprintf "warning unterminated comment@." }
  | _     { comment lexbuf }
