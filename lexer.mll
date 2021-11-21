{

open Parser

}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf } (* skip whitespaces *)
  | "def" { DEF }
  | "#" { COMMENT }
  | "*" { TYPE }
  | "¤" { KIND }
  | "@" { AXIOM }
  | "." { DOT }
  | ":=" { ASSIGN }
  | ":" { COLON }
  | "\\" { LAMBDA }
  | "/\\" { PRODUCT }
  | "(" { LPAR }
  | ")" { RPAR }
  | ";" { SEMICOLON }
  | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { VAR_IDENT id }
  | ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { CST_IDENT id }
  | eof { EOF }
