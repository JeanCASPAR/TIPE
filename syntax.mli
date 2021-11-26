module AssocTable: Map.S with type key = string

type tok_term =
  | Var of string
  | Sort of Lambda.sort
  | Apply of tok_term * tok_term
  | Abstraction of string * tok_term * tok_term
  | ProductType of string * tok_term * tok_term
  | Constant of string

type tok_def = {
  name : string;
  body : tok_term option;
  ty : tok_term;
}

(* the AssocTable contains the constant bindings *)
val translate_term : int AssocTable.t -> tok_term -> Lambda.term

(* idem *)
val translate_def : int AssocTable.t -> tok_def -> string * Lambda.definition
