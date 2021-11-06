%{

open Syntax

%}

%token<string> IDENT (* alphanumerical ascii, starts with a letter *)
%token DEF (* def *)
%token COMMENT (* # *)
%token TYPE_SORT (* * *)
%token KIND_SORT (* ¤ *)
%token AXIOM (* @ *)
%token DOT (* . *)
%token COLON (* : *)
%token COMMA (* , *)
%token LAMBDA (* \ *)
%token PRODUCT (* /\ *)
%token ASSIGN (* := *)
%token LPAR (* ( *)
%token RPAR (* ) *)
%token LBRA (* [ *)
%token RBRA (* ] *)
%token SEMICOLON (* ; *)
%token EOF

%start<Syntax.tok_definition list> parse_file

%nonassoc IDENT TYPE_SORT KIND_SORT DOT LAMBDA PRODUCT LPAR
%left APP

%%

expr:
  | LPAR ; e = expr; RPAR { e }
  | TYPE_SORT { Sort Lambda.Type }
  | KIND_SORT { Sort Lambda.Kind }
  | e1 = expr; e2 = expr; %prec APP { Apply (e1, e2) }
  | LAMBDA ; v = var_intro ; DOT ; e = expr
    { let (s, ty) = v in Abstraction (s, ty, e) }
  | PRODUCT ; v = var_intro; DOT ; e = expr
    { let (s, ty) = v in ProductType (s, ty, e) }
  | c = IDENT; LBRA ; l  = rev(separated_list(COMMA, expr)) ; RBRA {
      Constant (c, l)
  }
  | s = IDENT { Var s }

var_intro:
  LPAR ; s = IDENT ; COLON ; ty = expr ; RPAR { (s, ty) }

expr_or_axiom:
  | e = expr; { Some e }
  | AXIOM; { None }

def:
  DEF ; c = IDENT ; LBRA ; l = rev(separated_list(COMMA, var_intro)) ; RBRA ; ASSIGN ;
  e = expr_or_axiom ; COLON ; ty = expr ; SEMICOLON {{
      name = c;
      var = l;
      expr = e;
      ty = ty;
  }}

parse_file:
  l = list(def); EOF { l }

%%
