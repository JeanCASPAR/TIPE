type sequent = {
    assumptions : Formula.formula list;
    conclusions : Formula.formula list;
};;

let display_sequent seq =
    let f x = String.concat ", " (List.map Formula.display x)
    in let a = f seq.assumptions
    and c = f seq.conclusions in
    a ^ "  |-  " ^ c;;

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
    | LExists of Formula.formula;;

type equality = EqualityRefl of int (* Var x = Var x*)
    | EqualitySubstitutionPrincip of (int * int * Formula.formula);; (* x = y, P[x/u] |- P[y/u]*)

type inference = Theorem of Formula.axiom_request
    | Logical of logical
    | Equality of equality;;

type proof = {
    top : (sequent * proof) list;
    bottom : sequent;
    inference : inference;
};;

let display_proof p =
    let t = String.concat "   " (List.map (fun a -> display_sequent (fst a)) p.top)
    and b = display_sequent p.bottom in
    t ^ "\n" ^ String.make (max (String.length t) (String.length b)) '-' ^ "\n" ^ b

let rec swap n m l = if n > m
then swap m n l else if n = m then l
else
    let rec a k l = match (k, l) with
    | 0, x :: t -> let (y, t') = b x (m - n) t
        in y :: t'
    | k, x :: t -> x :: a (k - 1) t
    | _ -> failwith "Index not in list"
    and b x k l = match (k, l) with
    | 0, y :: t -> y, x :: t
    | k, z :: t -> let (y, t') = b x (k - 1) t
        in y, z :: t'
    | _ -> failwith "Index not in list"
    in a n l;;

(* return () if ok, raise Failure else *)
let rec check_proof theorems p = match p.inference with
    Logical Axiom -> if p.top != []
        then failwith ("Axiom premises not empty : " ^ display_proof p)
        else begin match p.bottom with
        { assumptions = [a]; conclusions = [c]} when a = c -> ()
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical Cut -> begin match p.top with
        [
            ({assumptions = l; conclusions = a :: d}, p1) ;
            ({assumptions = b :: l'; conclusions = d'}, p2)
        ] when a = b && p.bottom = {
            assumptions = l @ l';
            conclusions = d @ d';
        } -> check_proof theorems p1 ; check_proof theorems p2
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LWeakening -> begin match (p.top, p.bottom) with
        [seq, p'], {assumptions = _ :: l; conclusions = d} when
            seq.assumptions = l && seq.conclusions = d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical RWeakening -> begin match (p.top, p.bottom) with
        [seq, p'], {assumptions = l; conclusions = _ :: d} when
            seq.assumptions = l && seq.conclusions = d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LContract -> begin match (p.top, p.bottom) with
        [seq, p'], {assumptions = a :: l; conclusions = d} when
            seq.assumptions = a :: a :: l && seq.conclusions = d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical RContract -> begin match (p.top, p.bottom) with
        [seq, p'], {assumptions = l; conclusions = a :: d} when
            seq.assumptions = l && seq.conclusions = a :: a :: d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical (LExchange (k, l)) -> begin match p.top with
        [seq, p'] when seq.assumptions = swap k l p.bottom.assumptions
            && seq.conclusions = p.bottom.conclusions -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical (RExchange (k, l)) -> begin match p.top with
        [seq, p'] when seq.assumptions = p.bottom.assumptions
            && seq.conclusions = swap k l p.bottom.conclusions -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LNeg -> begin match p.top with 
        [{assumptions = l; conclusions = a :: d}, p'] when
            p.bottom.assumptions = Formula.Neg a :: l
            && p.bottom.conclusions = d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical RNeg -> begin match p.top with 
        [{assumptions = a :: l; conclusions = d}, p'] when
            p.bottom.assumptions = l
            && p.bottom.conclusions = Formula.Neg a :: d -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LAnd -> begin match p.top with
        [{assumptions = a :: b :: l; conclusions = d}, p'] when
            p.bottom.assumptions = Formula.And (a, b) :: l
            && p.bottom.conclusions = d
            -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical RAnd -> begin match p.top with
        [
            {assumptions = l; conclusions = a :: d}, p1 ;
            {assumptions = l'; conclusions = b :: d'}, p2 ;
        ] when l = l' && d = d'
            && p.bottom.assumptions = l
            && p.bottom.conclusions = Formula.And (a, b) :: d
            -> check_proof theorems p1; check_proof theorems p2
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LOr -> begin match p.top with
        [
            {assumptions = a :: l; conclusions = d}, p1 ;
            {assumptions = b :: l'; conclusions = d'}, p2 ;
        ] when l = l' && d = d'
            && p.bottom.assumptions = Formula.Or (a, b) :: l
            && p.bottom.conclusions = d
            -> check_proof theorems p1; check_proof theorems p2
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical ROr -> begin match p.top with
        [{assumptions = l; conclusions = a :: b :: d}, p'] when
            p.bottom.assumptions = l
            && p.bottom.conclusions = Formula.And (a, b) :: d
            -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical LImplies -> begin match p.top with
        [
            {assumptions = l; conclusions = a :: d}, p1 ;
            {assumptions = b :: l'; conclusions = d'}, p2 ;
        ] when l = l' && d = d'
        && p.bottom.assumptions = Formula.Implies (a, b) :: l
        && p.bottom.conclusions = d
        -> check_proof theorems p1; check_proof theorems p2
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Logical RImplies -> begin match p.top with
        [{assumptions = a :: l; conclusions = b :: d}, p']
        when p.bottom.assumptions = l
        && p.bottom.conclusions = Formula.Implies (a, b) :: d
        -> check_proof theorems p'
        | _ -> failwith ("Sequent not matching : " ^ display_proof p)
        end
    | Theorem req -> if p.top == []
        && p.bottom.assumptions == []
        && p.bottom.conclusions == [Formula.axioms theorems req]
        then ()
        else failwith ("Sequent not matching : " ^ display_proof p)
    | Equality (EqualityRefl x) -> if p.top == []
        && p.bottom.assumptions == []
        && p.bottom.conclusions == [Formula.Equal (Formula.Var x, Formula.Var x)]
        then ()
        else failwith ("Sequent not matching : " ^ display_proof p)
    | Equality (EqualitySubstitutionPrincip (x, y, formula)) ->
        if p.top == []
        && p.bottom.assumptions == [
            Formula.Equal (Formula.Var x, Formula.Var y);
            Formula.substitute_var 0 (Formula.Var x) formula
        ]
        && p.bottom.conclusions == [
            Formula.substitute_var 0 (Formula.Var y) formula
        ]
        then ()
        else failwith ("Sequent not matching : " ^ display_proof p)
    | _ -> failwith "dunno"
;;

let p5' = {
    top = [];
    bottom = {
        assumptions = [Formula.Atom 1];
        conclusions = [Formula.Atom 1];
    };
    inference = Logical Axiom;
};;

let p4' = {
    top = [{
        assumptions = [Formula.Atom 1];
        conclusions = [Formula.Atom 1];
    }, p5'];
    bottom = {
        assumptions = [Formula.Atom 0; Formula.Atom 1];
        conclusions = [Formula.Atom 1];
    };
    inference = Logical LWeakening;
};;

let p3' = {
    top = [{
        assumptions = [Formula.Atom 1; Formula.Atom 0];
        conclusions = [Formula.Atom 1];
    }, p4'];
    bottom = {
        assumptions = [Formula.Atom 0];
        conclusions = [Formula.Neg (Formula.Atom 1); Formula.Atom 1];
    };
    inference = Logical RNeg;
};;

let p5 = {
    top = [];
    bottom = {
        assumptions = [Formula.Atom 0];
        conclusions = [Formula.Atom 0];
    };
    inference = Logical Axiom;
};;

let p4 = {
    top = [{
        assumptions = [Formula.Atom 0];
        conclusions = [Formula.Atom 0];
    }, p5];
    bottom = {
        assumptions = [Formula.Atom 0];
        conclusions = [Formula.Atom 1; Formula.Atom 0];
    };
    inference = Logical RWeakening;
};;

let p3 = {
    top = [{
        assumptions = [Formula.Atom 0];
        conclusions = [Formula.Atom 0; Formula.Atom 1];
    }, p4];
    bottom = {
            assumptions = [Formula.Neg (Formula.Atom 0); Formula.Atom 0];
            conclusions = [Formula.Atom 1];
    };
    inference = Logical LNeg;
}

let p2 = {
    top = [
        {
            assumptions = [Formula.Atom 0];
            conclusions = [Formula.Neg (Formula.Atom 1); Formula.Atom 1];
        }, p3';
        {
            assumptions = [Formula.Neg (Formula.Atom 0); Formula.Atom 0];
            conclusions = [Formula.Atom 1];
        }, p3
    ];
    bottom = {
        assumptions = [
            Formula.Implies (Formula.Neg (Formula.Atom 1), Formula.Neg (Formula.Atom 0));
            Formula.Atom 0
        ];
        conclusions = [ Formula.Atom 1 ];
    };
    inference = Logical LImplies;
};;

let p1 = {
    top = [
        {
            assumptions = [
                Formula.Atom 0;
                Formula.Implies (Formula.Neg (Formula.Atom 1), Formula.Neg (Formula.Atom 0))
            ];
            conclusions = [ Formula.Atom 1 ];
        }, p2;
    ];
    bottom = {
        assumptions = [
            Formula.Implies (Formula.Neg (Formula.Atom 1), Formula.Neg (Formula.Atom 0))
        ];
        conclusions = [ Formula.Implies (Formula.Atom 0, Formula.Atom 1) ];
    };
    inference = Logical RImplies;
};;

let p = {
    top = [
        {
            assumptions = [
                Formula.Implies (Formula.Neg (Formula.Atom 1), Formula.Neg (Formula.Atom 0))
            ];
            conclusions = [ Formula.Implies (Formula.Atom 0, Formula.Atom 1) ];
        }, p1
    ];
    bottom = {
        assumptions = [];
        conclusions = [Formula.Implies (
            Formula.Implies (Formula.Neg (Formula.Atom 1), Formula.Neg (Formula.Atom 0)),
            Formula.Implies (Formula.Atom 0, Formula.Atom 1)
        )];
    };
    inference = Logical RImplies;
};;

let t = Hashtbl.create 0
in List.map (check_proof t) [
    p5; p5'; p4; p4'; p3; p3'; p2; p1; p
];;
print_endline (display_proof p);;
