type sequent = {
    assumptions : Formula.formula list;
    conclusions : Formula.formula list;
};;

let display_sequent seq =
    let f x = List.fold_right (fun a -> Formula.display a ^ ", ") x ""
    in let a = f seq.assumptions
    and c = f seq.conclusions in
    a ^ "|-" ^ c;;

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

;;

type inference = Theorem of Formula.axiom_request
    | Logical of logical;;

type proof = {
    top : (sequent * proof) list;
    bottom : sequent;
    inference : inference;
};;

let display_proof p =
    let t = List.fold_right (fun a -> display_sequent (fst a) ^ "   ") p.top ""
    and b = display_sequent p.bottom in
    t ^ "\n" ^ String.make (max (String.length t) (String.length b)) '-' ^ "\n"


(* return () if ok, raise Failure else *)
let rec check_proof theorems p = match p.inference with
    Axiom -> if p.top != []
        then failwith ("Axiom premises not empty : " ^ display_proof p)
        else begin match p.bottom with
        { assumptions = [a]; conclusions = [c]} when a = c -> ()
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
;;
