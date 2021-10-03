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

type axiom_request =
    (* Peano axiom number k
    See pdf file *)
    Peano of int
    (* Recurrence axiom scheme *)
    | Recurrence of formula
    (* Already proved theorems which name is registered *)
    | Theorem of string

(* take a list of theorems and a axiom request and return, if found,
  the corresponding formula
  Raise Not_Found else *)
val axioms : (string, formula) Hashtbl.t -> axiom_request -> formula
(* replace formula (Atom idx) with formula A in formula F *)
val substitute_prop : int -> formula -> formula -> formula
(* replace term (Var idx) with term y in formula F,
  where idx is free *)
val substitute_var : int -> term -> formula -> formula
val display : formula -> string
