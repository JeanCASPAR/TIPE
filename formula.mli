type term =
  Var of int
  | Zero
  | Succ of term
  | Add of (term * term)
  | Mul of (term * term)
type formula =
  Atom of int | Neg of formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Implies of (formula * formula)
  | Forall of formula
  | Exists of formula
  | Equal of (term * term)
type axiom_request = private
    Peano of int
    | Recurrence of (int * (term list -> formula))
    | Theorem of string;;
type rec_builder =
  RAtom of int | RFree of int (* nth free variable in this expression, >= 1 *)
  | RNeg of rec_builder
  | RAnd of (rec_builder * rec_builder)
  | ROr of (rec_builder * rec_builder)
  | RImplies of (rec_builder * rec_builder)
  | RForall of rec_builder
  | RExists of rec_builder
  | REqual of (term * term);;

(* return the Peano variant *)
val build_peano : int -> axiom_request
(* return the Recurrence variant *)
val build_rec : rec_builder -> axiom_request
(* return the Theorem variant *)
val build_theorem : string -> axiom_request

(* take a list of theorems and a axiom request and return, if found,
  the corresponding formula
  Raise Not_Found else *)
val axioms : (string, formula) Hashtbl.t -> axiom_request -> formula
(* replace formula A with formula B in formula C
  if A is not Atomic, user should ensure than A = B *)
val substitute_prop : formula -> formula -> formula -> formula
(* replace Var idx with term y in formula F,
  where idx is free *)
val substitue_var : int -> term -> formula -> formula
val display : formula -> string
