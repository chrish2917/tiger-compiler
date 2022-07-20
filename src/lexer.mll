{
open Parser
open Lexing

exception SyntaxError of String
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit= ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white      { read lexbuf }
  | newline    { next_line lexbuf; read lexbuf }
  | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id         { ID }
  | "while"    { WHILE }
  | "for"      { FOR }
  | "to"       { TO }
  | "break"    { BREAK }
  | "let"      { LET }
  | "in"       { IN }
  | "end"      { END }
  | "function" { FUNCTION }
  | "var"      { VAR }
  | "type"     { TYPE }
  | "array"    { ARRAY }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "do"       { DO }
  | "of"       { OF }
  | "nil"      { NIL }
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | ','        { COMMA }
  | ':'        { COLON }
  | ';'        { SEMICOLON }
  | '('        { LPARENS }
  | ')'        { RPARENS }
  | '['        { LBRACK }
  | ']'        { RBRACK }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '.'        { DOT }
  | '+'        { ADD }
  | '-'        { SUB }
  | '*'        { MUL }
  | '/'        { DIV }
  | '='        { EQ }
  | "<>"       { NEQ }
  | '<'        { LT }
  | "<="       { LE }
  | '>'        { GT }
  | ">="       { GE }
  | '&'        { AND }
  | '|'        { OR }
  | ":="       { ASSIGN }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }
