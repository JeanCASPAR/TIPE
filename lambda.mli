type sort = Type | Kind;; (* * | ¤ *)

type expr =
  Var of var (* binded variable with his type, using de Bruijn indices *)
  | Sort of sort
  | Apply of (expr * expr) (* M N *)
  | Abstraction of (var * expr) (* (Var (x, A), m) = \x : A. m; m : * *)
  | ProductType of (var * expr) (* (Var (x, A), B) = /\x : A. B; B : ¤ *)
  | Constant of (string * expr list) (* C[Un, ..., ... U1] *)
and var = int * expr (* id * type *)

val substitute : var -> expr -> expr -> expr

type definition = {
  name : string;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  var : var list;
  expr : expr option; (* None is @ *)
  ty : expr;
}

type interpreter = {
  (* liste D_n :: D_(n-1) ... D_1 :: [] *)
  (* chaque D_k peut dépendre des D_l, l < k *)
  definitions : definition list;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  context : var list;
  (* jugement : expr * expr; (* M : N, M et N dépendent des x_k *) *)
}

val check_program : interpreter -> definition list -> interpreter
val empty : interpreter

val show : definition -> string