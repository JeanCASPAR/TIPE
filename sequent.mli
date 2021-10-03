type sequent = {
    assumptions : Formula.formula list;
    conclusions : Formula.formula list;
}

val display_sequent : sequent -> string

(* see https://fr.wikipedia.org/wiki/Calcul_des_s%C3%A9quents#Le_syst%C3%A8me_LK *)
type logical =
    (* Identity group *)
    Axiom
    | Cut
    (* Structural group *)
    | LWeakening
    | RWeakening
    | LContract
    | RContract
    | LExchange of (int * int) (* exchange order of k and l *)
    | RExchange of (int * int)
    (* Logical group *)
    | LNeg
    | RNeg
    | LAnd
    | RAnd
    | LOr
    | ROr
    | LImplies
    | RImplies
    | LForall of Formula.formula (* the formula the variable is replaced with *)
    | RForall
    | RExists
    | LExists of Formula.formula

type inference = Theorem of Formula.axiom_request
    | Logical of logical

type proof = {
    top : (sequent * proof) list;
    bottom : sequent;
    inference : inference;
}

val display_proof : proof -> string

val check_proof : (string, Formula.formula) Hashtbl.t -> proof -> unit