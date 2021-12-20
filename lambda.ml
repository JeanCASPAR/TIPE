type sort = Type | Kind (* * | ¤ *)

type term =
  Var of int (* de Bruijn indices, starts at 1 *)
  | Sort of sort
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

(* substitute n a b = a[n:=b] *)
let substitute n a b = subst (function
| k when k = n -> b
| k -> Var k) a;;

(* it preserves well-typing *)
let rec reduce = function
| Abstraction (a, b) -> Abstraction (reduce a, reduce b)
| ProductType (a, b) -> ProductType (reduce a, reduce b)
| Apply (a, b) -> begin match reduce a with
  | Abstraction (_, t) -> substitute 1 t (reduce b)
  | ProductType (_, t) -> substitute 1 t (reduce b)
  | t -> Apply (reduce t, reduce b)
end
| t -> t

type definition = {
  body : term option; (* None stands for an axiom. It is written @ in the grammar *)
  ty : term;
}

type state = {
  defs : definition array; (* [| D_1, ..., D_n |] array of previously verified definitions, D_k corresponds to Constant k *)
  vars : term list (* list A_n, ..., A_i, ..., A_1 of the type A_i of the i-th free var *)
}

let rec show_term = function
| Var n -> string_of_int n
| Sort Type -> "*"
| Sort Kind -> "¤"
| Apply (a, b) -> "(" ^ show_term a ^ " " ^ show_term b ^ ")"
| Abstraction (a, b) -> "( \\ " ^ show_term a ^ " . " ^ show_term b ^ ")"
| ProductType (a, b) -> "( /\\ " ^ show_term a ^ " . " ^ show_term b ^ ")"
| Constant n -> "[" ^ string_of_int n ^ "]"

let show_def state =
  let s = ref "" in
  for i = 0 to Array.length state.defs - 1 do
    let def = state.defs.(i) in
    s := !s ^ "def [" ^ string_of_int i ^ "] := " ^ (match def.body with Some t -> show_term t | None -> "@") ^ " : " ^ show_term def.ty ^ ";\n"
  done;
  !s

let show state term ty =
  print_newline ();
  print_string (show_def state);
  List.iter (fun t -> print_endline (show_term t ^ ";")) (List.rev state.vars);
  print_endline ("|- " ^ (match term with Some t -> show_term t | None -> "@") ^ " : " ^ show_term ty ^ " §");
  print_newline ()

type error =
  | NotTypable of term * string (* t, explanation *)
  | IsNotASort of term * term (* t, ty where t : ty and ty should be a sort *)
  | ConstantOutOfBound of int (* k with Constant k which don't exists *)
  | TypesDoNotMatch of term * term * string (* t1, t2, explanation where t1 <> t2 *)
  | NotClosedTerm of int (* k with Var k which isn't binded *)
exception Error of error

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
  | [] -> failwith "pop from empty context"

(* we can massively use reduce because we know the term are well-typed *)
let rec typing state t = begin match reduce t with
| Var k ->
  let rec incr_by_n n t = match n with
  | 0 -> t
  | n -> incr_by_n (n - 1) (incr t)
  in
  (* we retrieve the type of t *)
  let ty = List.nth state.vars (k - 1) in
  (* we adapt the type to the current context *)
  (* let vars = Array.of_list (
    List.filteri (fun i _ -> i >= k) state.vars) in *)
  incr_by_n k ty
| Sort Type -> Sort Kind
| Sort Kind -> raise (Error (NotTypable (Sort Kind,
  "[This state shouldn't be reached] term Kind does not have a type")))
| Apply (a, b) -> begin match typing state a with
  | ProductType (x, y) -> Apply (ProductType (x, y), b) (* it is reduced after *)
  | t -> raise (Error (NotTypable (Apply (a, b),
    "[This state shouldn't be reached] " ^
    "you can't apply a on b with a = " ^ show_term a ^ " : " ^ show_term t ^
    " and b = " ^ show_term b)))
end
| Abstraction (a, b) -> ProductType (a, typing (push_var a state) b)
| ProductType (a, b) -> typing (push_var a state) b
| Constant k -> state.defs.(k).ty
end |> reduce


let fail_if_not_sort state t = match typing state t with
| Sort _ -> ()
| ty -> raise (Error (IsNotASort (t, ty)))

let rec check state = let n = List.length state.vars in function
| Var k when k <= n ->
  if k = 0
  then
    failwith "k = 0";

  let (ty, st) = pop_var state in (* safe because k >= 1*)
  check st ty;
  fail_if_not_sort st ty;

  if k = 1
  then begin
    let t1 = typing state (Var k)
    and t2 = reduce (incr ty) (* ty live in a context with one less variable *)
    in if t1 <> t2
    then raise (Error (TypesDoNotMatch (t1, t2, "Var " ^ string_of_int k ^
    " should be of type t2 = " ^ show_term t2 ^ ", not t1 = " ^ show_term t1)))
  end else
    check st (Var (k - 1))
| Var k (* k > n *) -> raise (Error (NotClosedTerm k)) (* the term is not closed *)
| Sort Type when state.vars = [] -> ()
| Sort Type ->
  let (ty, st) = pop_var state in
  check st ty;
  fail_if_not_sort st ty;
  check st (Sort Type);
| Sort Kind -> raise (Error (NotTypable (Sort Kind, "term Kind does not have a type")))
| Apply (a, b) ->
  check state a;
  check state b;
  let x = typing state b in
  begin match typing state a with
  | ProductType (y, _) when y = x -> ()
  | t -> raise (Error (NotTypable (Apply (a, b),
    "you can't apply a on b with a = " ^ show_term a ^ " : " ^ show_term t ^
    " and b = " ^ show_term b ^ " : " ^ show_term x)))
  end
| Abstraction (a, b) ->
  let new_state = push_var a state in
  check new_state b;
  let c = typing new_state b in
  check state (ProductType (a, c));
  fail_if_not_sort state (ProductType (a, c));
| ProductType (a, b) ->
  check state a;
  fail_if_not_sort state a;
  let new_state = push_var a state in
  check new_state b;
  fail_if_not_sort new_state b;
| Constant k -> if (k >= Array.length state.defs)
  then raise (Error (ConstantOutOfBound k))


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

    let t = ProductType (Constant 0, Apply (ProductType (Constant 0, Constant 0), Var 1)) in
    assert (reduce t = ProductType (Constant 0, Constant 0));
    let t = Apply (Apply (Abstraction (Sort Type, Abstraction (Sort Type,
      ProductType (Var 2, Var 2))), Constant 0), Constant 0) in
    assert (reduce t = ProductType (Constant 0, Constant 0));
    ()

  let test_typing () =
    let state = {
      defs = [||];
      vars = [Var 1; Sort Type];
    } in
    let term = Var 1
    in print_endline ("@" ^ show_term (typing state term));
end
