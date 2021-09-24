type var =
  Var of int
  | Zero
  | Succ of var
  | Add of (var * var)
  | Mul of (var * var)
type formula =
  Atom of int | Neg of formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Implies of (formula * formula)
  | Forall of formula
  | Exists of formula
  | Equal of (var * var)
type axiom_request = 
    Peano of int
    | private Recurrence of (int * (var list -> formula))
    | Theorem of string;;
type rec_builder =
  RAtom of int | RFree of int (* nth free variable in this expression, >= 1 *)
  | RNeg of rec_builder
  | RAnd of (rec_builder * rec_builder)
  | ROr of (rec_builder * rec_builder)
  | RImplies of (rec_builder * rec_builder)
  | RForall of rec_builder
  | RExists of rec_builder
  | REqual of (var * var);;

(* return the Recurrence variant *)
val build : rec_builder -> axiom_request

val axioms : (string, formula) Hashtbl.t -> axiom_request -> formula
val substitute : formula -> formula -> formula
val display : formula -> string