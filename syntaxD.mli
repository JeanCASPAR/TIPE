module AssocTable: Map.S with type key = string

type tok_term =
  | Var of string
  | Sort of LambdaD.sort
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
val translate_term : int AssocTable.t -> tok_term -> LambdaD.term

(* idem *)
val translate_def : int AssocTable.t -> tok_def -> string * LambdaD.definition
