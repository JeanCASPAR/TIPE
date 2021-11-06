type tok_expr =
  Var of string
  | Sort of Lambda.sort
  | Apply of (tok_expr * tok_expr) (* M N *)
  | Abstraction of (string * tok_expr * tok_expr) (* (x, A, m) = \x : A. m; m : * *)
  | ProductType of (string * tok_expr * tok_expr) (* (x, A, B) = /\x : A. B; B : ¤ *)
  | Constant of (string * tok_expr list) (* C[Un, ..., ... U1] *)

type tok_definition = {
  name : string;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  var : (string * tok_expr) list;
  expr : tok_expr option; (* None is @ *)
  ty : tok_expr;
}

type syntax_error = UnknownVariable of string | Eof
exception SyntaxError of syntax_error
val convert_list_def : tok_definition list -> Lambda.definition list