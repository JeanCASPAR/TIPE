%{

open Syntax

%}

%token<string> VAR_IDENT (* alphanumerical ascii, starts with a lowercase letter *)
%token<string> CST_IDENT (* alphanumerical ascii, starts with an uppercase letter *)
%token DEF (* def *)
%token COMMENT (* # *)
%token TYPE (* * *)
%token KIND (* ¤ *)
%token AXIOM (* @ *)
%token DOT (* . *)
%token COLON (* : *)
%token LAMBDA (* \ *)
%token PRODUCT (* /\ *)
%token ASSIGN (* := *)
%token LPAR (* ( *)
%token RPAR (* ) *)
%token SEMICOLON (* ; *)
%token EOF

%start<Syntax.tok_def list> parse_file

%right DOT
%nonassoc VAR_IDENT CST_IDENT TYPE KIND LAMBDA PRODUCT LPAR
%left APP

%%

expr:
  | LPAR ; e = expr; RPAR { e }
  | TYPE { Sort Lambda.Type }
  | KIND { Sort Lambda.Kind }
  | e1 = expr; e2 = expr; %prec APP { Apply (e1, e2) }
  | LAMBDA ; v = var_intro ; DOT ; e = expr %prec DOT
    { let (s, ty) = v in Abstraction (s, ty, e) }
  | PRODUCT ; v = var_intro; DOT ; e = expr %prec DOT
    { let (s, ty) = v in ProductType (s, ty, e) }
  | c = CST_IDENT { Constant c }
  | s = VAR_IDENT { Var s }

var_intro:
  LPAR ; s = VAR_IDENT ; COLON ; ty = expr ; RPAR { (s, ty) }

expr_or_axiom:
  | e = expr { Some e }
  | AXIOM { None }

def:
  DEF ; c = CST_IDENT ; ASSIGN ;
  e = expr_or_axiom ; COLON ; ty = expr ; SEMICOLON {{
      name = c;
      body = e;
      ty = ty;
  }}

parse_file:
  l = list(def); EOF { l }

%%
