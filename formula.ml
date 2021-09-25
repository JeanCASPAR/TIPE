(* with de bruijn index for var *)
type var =
  Var of int
  | Zero
  | Succ of var
  | Add of (var * var)
  | Mul of (var * var);;
type formula =
  Atom of int | Neg of formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Implies of (formula * formula)
  | Forall of formula
  | Exists of formula
  | Equal of (var * var);;

type axiom_request = 
    Peano of int
    | Recurrence of (int * (var list -> formula))
    | Theorem of string;;

let build_peano n = Peano n;;
let build_theorem s = Theorem s;;

type rec_builder =
  RAtom of int | RFree of int (* nth free variable in this expression, >= 1 *)
  | RNeg of rec_builder
  | RAnd of (rec_builder * rec_builder)
  | ROr of (rec_builder * rec_builder)
  | RImplies of (rec_builder * rec_builder)
  | RForall of rec_builder
  | RExists of rec_builder
  | REqual of (var * var);;

(* TODO: trop restrectif, devrait permettre de garder des variables libres *)
(* totalement faux, j'aurais du remplacer dans les var et pas dans les prop *)
let build_rec builder =
    let rec count = function
        | RAtom _ -> 0
        | RFree k -> k
        | RNeg a -> count a
        | RAnd (a, b) -> max (count a) (count b)
        | ROr (a, b) -> max (count a) (count b)
        | RImplies (a, b) -> max (count a) (count b)
        | RForall a -> count a
        | RExists a -> count a
        | REqual _ -> 0
    in let n = count builder in
    let rec substitute n l = function
        | RAtom k -> Atom k
        | RFree k -> l.(k)
        | RNeg a -> Neg (substitute n l a)
        | RAnd (a, b) -> And (substitute n l a, substitute n l b)
        | ROr (a, b) -> Or (substitute n l a, substitute n l b)
        | RImplies (a, b) -> Implies (substitute n l a, substitute n l b)
        | RForall a -> Forall (substitute n l a)
        | RExists a -> Exists (substitute n l a)
        | REqual a -> Equal a
    in let recurrent l =
        let arr = Array.of_list (List.map (function Var k -> Atom k | _ -> failwith "nope") l) in
        if Array.length arr != n
        then failwith "erreur"
        else substitute n arr builder
    in Recurrence (n, recurrent);;


let axioms theorems = function
    Peano 0 -> Forall (Neg (Equal (Succ (Var 0), Zero)))
    | Peano 1 -> Forall (And (
        Equal (Var 0, Zero),
        Exists (Equal (Var 1, Succ (Var 0))))
    )
    | Peano 2 -> Forall (Forall (Implies (
        Equal (Succ (Var 1), Succ (Var 0)),
        Equal (Var 1, Var 0)
    )))
    | Peano 3 -> Forall (Equal (Add (Var 0, Zero), Var 0))
    | Peano 4 -> Forall (Forall (Equal (
        Add (Var 1, Succ (Var 0)),
        Succ (Add (Var 1, Var 0))
    )))
    | Peano 5 -> Forall (Equal (Mul (Var 0, Zero), Zero))
    | Peano 6 -> Forall (Forall (Equal (
        Mul (Var 1, Succ (Var 0)),
        Add (Mul (Var 1, Var 0), Var 1)
    )))
    | Recurrence (n, phi) ->
        let rec free_vars = function
        | 0 -> []
        | k -> Var (n - k) :: free_vars (k - 1)
        and forall_builder p = function
        | 0 -> p
        | k -> Forall (forall_builder p (k - 1)) in
        let f = free_vars (n - 1) in
        let g = List.map (function (Var n) -> Var (n + 1) | _ -> failwith "nope") f in
        forall_builder (Implies (
            And (phi (Zero :: f), Forall (Implies (
                phi (Var 0 :: g),
                phi (Succ (Var 0) :: g)
            ))),
            Forall (phi (Var 0 :: g))
        )) n
    | Theorem name -> Hashtbl.find theorems name
    | _ -> raise Not_found;;

let incr k =
    (* k : de combien à augmenter *)
    (* n : nombre de binders devant *)
    let rec incr k n = function
    Atom l -> if l < n then Atom l else Atom (l + k)
    | Neg p -> Neg (incr k n p)
    | And (p, q) -> And (incr k n p, incr k n q)
    | Or (p, q) -> Or (incr k n p, incr k n q)
    | Implies (p, q) -> Implies (incr k n p, incr k n q)
    | Forall p -> Forall (incr k (n + 1) p)
    | Exists p -> Forall (incr k (n + 1) p)
    | Equal (x, y) -> Equal (x, y) in
    incr k 0;;

let substitute a b = match a with
Forall a ->
    (* n est le nombre de binders devant le terme *)
    let rec aux_substitute n = function
    Atom l -> if l < n
        then Atom l (* variable liée *)
        else if l = n (* variable à remplacer *)
        then incr n b (* on ajouter des binders devant b
        donc on monte les index des variables libres dans b *)
        else Atom (l - 1) (* on enlève un binder donc les variables libres baissent *)
    | Neg p -> Neg (aux_substitute n p)
    | And (p, q) -> And (aux_substitute n p, aux_substitute n q)
    | Or (p, q) -> Or (aux_substitute n p, aux_substitute n q)
    | Implies (p, q) -> Implies (aux_substitute n p, aux_substitute n q)
    (* On ajouter un binder donc on les variables libres sont plus hautes *)
    | Forall p -> Forall (aux_substitute (n + 1) p)
    | Exists p -> Exists (aux_substitute (n + 1) p)
    | Equal (x, y) -> Equal (x, y)
    in
    let tmp = incr (-1) a in
    aux_substitute 0 tmp
| _ -> failwith "not substituable";;

let display =
    (* génère l'ensemble des mots sur l'alphabet usuel, par longueurs croissantes
    et ordre lexicographique ensuite *)
    let rec free_vars n =
        if n < 26
        then String.make 1 (Char.chr (Char.code 'a' + n))
        else
            let (b, r) = (n / 26, n mod 26) in
            free_vars (b - 1) ^ free_vars r
    in
    (* n : nb de binder devant *)
    let rec display_var n = function
        Var k -> if k < n then free_vars (2 * (n - k) - 1) else free_vars (2 * (k - n))
        | Zero -> "0"
        | Succ v -> "S" ^ display_var n v
        | Add (x, y) -> "(" ^ display_var n x ^ " + " ^ display_var n y ^ ")"
        | Mul (x, y) -> "(" ^ display_var n x ^ " * " ^ display_var n y ^ ")"
    in
    (* n : nombre de binders *)
    let rec aux_display n = function
    Atom k -> String.uppercase_ascii (free_vars k)
    | Neg p -> "-" ^ aux_display n p
    | And (p, q) -> "(" ^ aux_display n p ^ " /\\ " ^ aux_display n q ^ ")"
    | Or (p, q) -> "(" ^ aux_display n p ^ " \\/ " ^ aux_display n q ^ ")"
    | Implies (p, q) -> "(" ^ aux_display n p ^ " => " ^ aux_display n q ^ ")"
    (* On rajoute un binder *)
    | Forall p -> "(forall " ^ free_vars (2 * n + 1) ^ " . " ^ aux_display (n + 1) p ^ ")"
    | Exists p -> "(exists " ^ free_vars (2 * n + 1) ^ " . " ^ aux_display (n + 1) p ^ ")"
    | Equal (x, y) -> "(" ^ display_var n x ^ " = " ^ display_var n y ^ ")"
    in aux_display 0;;

let t = And (Forall (Equal (Succ (Var 0), Var 1)), Atom 0)
in print_endline (display t);;
