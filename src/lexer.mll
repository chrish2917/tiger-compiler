{
open Parser
open Lexing

exception SyntaxError of String
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit= ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
