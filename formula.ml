(* with de bruijn index for Var *)
type term =
  Var of int
  | Zero
  | Succ of term
  | Add of (term * term)
  | Mul of (term * term);;

type formula =
  Atom of int | Neg of formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Implies of (formula * formula)
  | Forall of formula
  | Exists of formula
  | Equal of (term * term);;


(* replace Var idx with term y in formula F,
  where idx is free *)
let substitute_var idx term formula =
    (* n : nombre de binder devant la variable
           à substituer 
       k : nombre de binder dans le terme devant le point actuel
    *)
    let rec incr_term n k = function
    Var idy -> if idy <= k
        then Var idy (* idy est liée*)
        else Var (idy + n) (* idy est libre et on a rajouté n binders devant *)
    | Zero -> Zero
    | Succ x -> Succ (incr_term n k x)
    | Add (x, y) -> Add (incr_term n k x, incr_term n k y)
    | Mul (x, y) -> Mul (incr_term n k x, incr_term n k y)

    in let rec update_term n k = function
    Var idy -> if idy = idx + n + k (* si on a trouvé la variable libre qu'on cherchait*)
        then incr_term n k term (* on remplace idy
        en incrémentant les variables libres de term *)
        else Var idy
    | Zero -> Zero
    | Succ x -> Succ (update_term n k x)
    | Add (x, y) -> Add (update_term n k x, update_term n k y)
    | Mul (x, y) -> Mul (update_term n k x, update_term n k y)
    
    in let rec update_prop n = function
    Atom idy -> Atom idy
    | Neg p -> Neg (update_prop n p)
    | And (p,q) -> And (update_prop n p, update_prop n q)
    | Or (p,q) -> Or (update_prop n p, update_prop n q)
    | Implies (p, q) -> Implies (update_prop n p, update_prop n q)
    | Forall p -> Forall (update_prop (n + 1) p)
    | Exists p -> Exists (update_prop (n + 1) p)
    | Equal (x, y) -> Equal (update_term n 0 x, update_term n 0 y)

    in update_prop 0 formula;;

(* replace formula A with formula B in formula C
  if A is not Atomic, user should ensure than A = B *)
let substitute_prop idx a f =
    let rec replace_term n k = function
    Var idy -> if idy <= k
        then Var idy (* la variable est liée *)
        else Var (idy + n) (* on a rajouté n binders devant*)
    | Zero -> Zero
    | Succ x -> Succ (replace_term n k x)
    | Add (x, y) -> Add (replace_term n k x, replace_term n k y)
    | Mul (x, y) -> Mul (replace_term n k x, replace_term n k y)

    in let rec replace_prop n k = function
    Atom idy -> Atom idy
    | Neg p -> Neg (replace_prop n k p)
    | And (p, q) -> And (replace_prop n k p, replace_prop n k q)
    | Or (p, q) -> Or (replace_prop n k p, replace_prop n k q)
    | Implies (p, q) -> Implies (replace_prop n k p, replace_prop n k q)
    | Forall p -> Forall (replace_prop n (k + 1) p)
    | Exists p -> Exists (replace_prop n (k + 1) p)
    | Equal (x, y) -> Equal (replace_term n k x, replace_term n k y)

    in let rec find_prop n = function
    Atom idy -> if idy = idx
        then replace_prop n 0 a (* si on a trouvé un atome à remplacer *)
        else Atom idy
    | Neg p -> Neg (find_prop n p)
    | And (p, q) -> And (find_prop n p, find_prop n q)
    | Or (p, q) -> Or (find_prop n p, find_prop n q)
    | Implies (p, q) -> Implies (find_prop n p, find_prop n q)
    | Forall p -> Forall (find_prop (n + 1) p)
    | Exists p -> Exists (find_prop (n + 1) p)
    | Equal (x, y) -> Equal (x, y)
    
    in find_prop 0 f;;

type axiom_request = 
    Peano of int
    | Recurrence of formula
    | Theorem of string;;

let axioms theorems = function
    Peano 1 -> Forall (Neg (Equal (Succ (Var 0), Zero)))
    | Peano 21 -> Forall (And (
        Equal (Var 0, Zero),
        Exists (Equal (Var 1, Succ (Var 0))))
    )
    | Peano 3 -> Forall (Forall (Implies (
        Equal (Succ (Var 1), Succ (Var 0)),
        Equal (Var 1, Var 0)
    )))
    | Peano 4 -> Forall (Equal (Add (Var 0, Zero), Var 0))
    | Peano 5 -> Forall (Forall (Equal (
        Add (Var 1, Succ (Var 0)),
        Succ (Add (Var 1, Var 0))
    )))
    | Peano 6 -> Forall (Equal (Mul (Var 0, Zero), Zero))
    | Peano 7 -> Forall (Forall (Equal (
        Mul (Var 1, Succ (Var 0)),
        Add (Mul (Var 1, Var 0), Var 1)
    )))
    | Recurrence formula -> begin
        let rec count_free_var_term n = function
        | Var idx -> if idx <= n
            then 0
            else idx + 1 (* we start at 1 since 0 means no free variable *)
        | Zero -> 0
        | Succ x -> count_free_var_term n x
        | Add (x, y) -> max (count_free_var_term n x) (count_free_var_term n y)
        | Mul (x, y) -> max (count_free_var_term n x) (count_free_var_term n y)
        
        in let rec count_free_var_prop n = function
            | Atom _ -> 0
            | Neg p -> count_free_var_prop n p
            | And (p, q) -> max (count_free_var_prop n p) (count_free_var_prop n q)
            | Or (p, q) -> max (count_free_var_prop n p) (count_free_var_prop n q)
            | Implies (p, q) -> max (count_free_var_prop n p) (count_free_var_prop n q)
            | Forall p -> count_free_var_prop (n + 1) p
            | Exists p -> count_free_var_prop (n + 1) p
            | Equal (x, y) -> max (count_free_var_term n x) (count_free_var_term n y)
        
        in let n = count_free_var_prop 0 formula

        in if n == 0
        then formula (* aucune variable libre*)
        else begin
            let n = n - 1 (* indice de la variable libre de plus haut rang,
                et nombre de variables libres != 0 *)
            in let f = substitute_var n Zero formula
            and g = substitute_var n (Var 0) formula
            in let h = Forall (Implies (
                g,
                substitute_var n (Succ (Var 0)) formula
            ))
            in let i = Implies (And (f, h), Forall h)
            in let rec iterate f x = function
            | 0 -> x
            | k -> iterate f (f x) (k - 1)
            in iterate (fun p -> Forall p) i n
        end
    end
    | Theorem name -> Hashtbl.find theorems name
    | _ -> raise Not_found;;

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
