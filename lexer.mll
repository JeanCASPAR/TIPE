{

open Parser

}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf } (* skip whitespaces *)
  | "def" { DEF }
  | "#" { COMMENT }
  | "*" { TYPE_SORT }
  | "¤" { KIND_SORT }
  | "@" { AXIOM }
  | "." { DOT }
  | ":=" { ASSIGN }
  | ":" { COLON }
  | "," { COMMA }
  | "\\" { LAMBDA }
  | "/\\" { PRODUCT }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRA }
  | "]" { RBRA }
  | ";" { SEMICOLON }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
  | eof { EOF }