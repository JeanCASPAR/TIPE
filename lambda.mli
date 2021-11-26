type sort = Type | Kind;; (* * | ¤ *)

type term =
  Var of int (* de Bruijn indices *)
  | Sort of sort
  | Apply of (term * term) (* M N *)
  | Abstraction of (term * term) (* (Var (x, A), m) = \x : A. m; m : * *)
  | ProductType of (term * term) (* (Var (x, A), B) = /\x : A. B; B : ¤ *)
  | Constant of int (* definition order *)


(* substitute n a b = a[n:=b] *)
val substitute : int -> term -> term -> term

(* preserves well-typedness, reduce application *)
val reduce : term -> term

type definition = {
  body : term option; (* None stands for an axiom. It is written @ in the grammar *)
  ty : term;
}

type state = {
  defs : definition array; (* [| D_1, ..., D_n |] array of previously verified definitions, D_k corresponds to Constant k *)
  vars : term list (* list A_n, ..., A_i, ..., A_1 of the type A_i of the i-th free var *)
}

type error =
  | NotTypable of term * string (* t, explanation *)
  | IsNotASort of term * term (* t, ty where t : ty and ty should be a sort *)
  | ConstantOutOfBound of int (* k with Constant k which don't exists *)
  | TypesDoNotMatch of term * term * string (* t1, t2, explanation where t1 <> t2 *)
  | NotClosedTerm of int (* k with Var k which isn't binded *)
exception Error of error

(* unfold all non-axiom constant definitions *)
val unfold : state -> term -> term

(* take a well-formed state and a well-typed term and returns its type *)
val typing : state -> term -> term

(* take a well-formed state and a term and checks its well-typedness *)
val check : state -> term -> unit

(* (Delta; Gamma) |- M or @ : N § *)
val show : state -> term option -> term -> unit

val show_term : term -> string