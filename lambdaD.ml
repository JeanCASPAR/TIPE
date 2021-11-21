type universe = Type | Kind (* * | ¤ *)

type term =
  Var of int (* de Bruijn indices, starts at 1 *)
  | Universe of universe
  | Apply of (term * term) (* M N *)
  | Abstraction of (term * term) (* (Var (x, A), m) = \x : A. m; m : * *)
  | ProductType of (term * term) (* (Var (x, A), B) = /\x : A. B; B : ¤ *)
  | Constant of int (* definition order *)

let rec subst s = function
| Var n -> s n
| Apply (a, b) -> Apply (subst s a, subst s b)
| Abstraction (a, b) -> Abstraction (subst s a, subst (function
  | 1 -> Var 1
  | n -> incr (s (n - 1))) b)
| ProductType (a, b) -> ProductType (subst s a, subst (function
  | 1 -> Var 1
  | n -> incr (s (n - 1))) b)
| t -> t
and incr t = subst (fun k -> Var (k + 1)) t

let substitute n a b = subst (function
| k when k = n -> b
| k -> Var k) a;;

(* it preserves well-typing *)
let reduce_step = function
| Apply (Abstraction (_, b), c) -> substitute 1 b c
| Apply (ProductType (_, b), c) -> substitute 1 b c
| t -> t

let rec reduce t = let g = reduce_step t in
if g = t then t
else reduce g;;

let is_universe t = match reduce t with
| Universe _ -> true
| _ -> false

let undef () = failwith "This operation is not defined"
let undef_if b = if b then undef ()

let nth_end n l =
  let rec aux k = function
  | [x] ->
    if k = 0
    then Some x, 1 (* we starts backwards at 1 *)
    else None, 1
  | h :: t -> let opt, idx = aux (k - 1) t in
    if Option.is_some opt then opt, idx + 1
    else if idx = k then Some h, idx + 1
    else None, idx + 1
  | _ -> undef ()
  in let opt, _ = aux n l
  in Option.get opt

type definition = {
  body : term option; (* None stands for an axiom. It is written @ in the grammar *)
  ty : term;
}

type state = {
  defs : definition array; (* [| D_1, ..., D_n |] array of previously verified definitions, D_k corresponds to Constant k *)
  vars : term list (* list A_n, ..., A_i, ..., A_1 of the type A_i of the i-th free var *)
}

(* unfold all non-axiom constant definitions *)
let rec unfold state = function
| Apply (a, b) -> Apply (unfold state a, unfold state b)
| Abstraction (a, b) -> Abstraction (unfold state a, unfold state b)
| ProductType (a, b) -> ProductType (unfold state a, unfold state b)
| Constant k -> begin match state.defs.(k).body with
  | Some t -> unfold state t
  | None -> Constant k
  end
| t -> t

let push_var t state = {state with vars = t :: state.vars};;
let pop_var state = match state.vars with
  | h :: t -> (h, {state with vars = t})
  | [] -> undef ()

(* we can massively use reduce because we know the term are well-typed *)
let rec typing state t = begin match reduce t with
| Var k -> List.nth (List.rev state.vars) k (* it *is* the correct order of indexing *)
| Universe Type -> Universe Kind
| Universe Kind -> undef ()
| Apply (a, b) -> begin match reduce (typing state a) with
  | ProductType (x, y) -> Apply (ProductType (x, y), b)
  | _ -> undef ()
end
| Abstraction (a, b) -> ProductType (a, b)
| ProductType (a, b) -> typing (push_var a state) b
| Constant k -> state.defs.(k).ty
end |> reduce

let rec check state = let n = List.length state.vars in function
| Var k when k = n -> let (ty, st) = pop_var state in (* safe because k >= 1*)
  check st ty;
  ty |> typing st |> is_universe |> not |> undef_if;
  undef_if (typing state (Var k) != reduce ty)
| Var k when k > n -> undef () (* the term is not closed *)
| Universe Type when state.vars = [] -> ()
| Universe Kind -> undef ()
| Apply (a, b) ->
  check state a;
  check state b;
  let x = typing state b in
  begin match typing state b with
  | ProductType (y, _) when y = x -> ()
  | _ -> undef ()
  end
| Abstraction (a, b) ->
  check (push_var a state) b;
  let c = typing (push_var a state) b in
  check state (ProductType (a, c));
  ProductType (a, b) |> typing state |> is_universe |> not |> undef_if
| ProductType (a, b) ->
  check state a;
  a |> typing state |> is_universe |> not |> undef_if;
  check (push_var a state) b;
  b |> typing (push_var a state) |> is_universe |> not |> undef_if
| Constant k -> undef_if (k >= Array.length state.defs)
(* the term is either a variable different from the last free variable of state.vars, or * : # with a non-empty list of free variables *)
| t -> let (ty, st) = pop_var state in
  check st t;
  check st ty;
  ty |> typing st |> is_universe |> not |> undef_if

module Test = struct
  let test_reduce =
    let t = Apply (Abstraction (Constant 0, Var 1), Constant 1) in
    assert (reduce t = Constant 1);
    let t = Apply (Abstraction (Constant 0, Var 1), Var 1) in
    assert (reduce t = Var 1);
    let t = Apply (Abstraction (Constant 0, Var 2), Var 1) in
    assert (reduce t = Var 2);
    let t = Apply (Abstraction (Constant 0, Var 1), Var 2) in
    assert (reduce t = Var 2);

    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 1)), Constant 2) in
    assert (reduce t = Abstraction (Constant 1, Var 1));
    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 2)), Constant 2) in
    assert (reduce t = Abstraction (Constant 1, Constant 2));
    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 2)), Var 1) in
    assert (reduce t = Abstraction (Constant 1, Var 2));
    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 2)), Var 2) in
    assert (reduce t = Abstraction (Constant 1, Var 3));

    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 2)), Abstraction (Constant 2, Var 1)) in
    assert (reduce t = Abstraction (Constant 1, Abstraction (Constant 2, Var 1)));
    let t = Apply (Abstraction (Constant 0, Abstraction (Constant 1, Var 2)), Abstraction (Constant 2, Var 2)) in
    assert (reduce t = Abstraction (Constant 1, Abstraction (Constant 2, Var 3)));
end

let rec show_term = function
| Var n -> string_of_int n
| Universe Type -> "*"
| Universe Kind -> "¤"
| Apply (a, b) -> "(" ^ show_term a ^ show_term b ^ ")"
| Abstraction (a, b) -> "( \\ " ^ show_term a ^ " . " ^ show_term b ^ ")"
| ProductType (a, b) -> "( /\\ " ^ show_term a ^ " . " ^ show_term b ^ ")"
| Constant n -> "[" ^ string_of_int n ^ "]"

let show_def state =
  let s = ref "" in
  for i = 0 to Array.length state.defs - 1 do
    let def = state.defs.(i) in
    s := !s ^ "def [" ^ string_of_int i ^ "] := " ^ match def.body with Some t -> show_term t | None -> "@" ^ " : " ^ show_term def.ty ^ ";\n"
  done;
  !s

let show state term ty =
  print_endline (show_def state);
  List.iter (fun t -> print_endline (show_term t ^ ";")) state.vars;
  print_endline ("|- " ^ (match term with Some t -> show_term t | None -> "@") ^ " : " ^ show_term ty ^ " §")
