module AssocTable = Map.Make(String)

type tok_term =
  | Var of string
  | Sort of Lambda.sort
  | Apply of tok_term * tok_term
  | Abstraction of string * tok_term * tok_term
  | ProductType of string * tok_term * tok_term
  | Constant of string

type tok_def = {
  name : string;
  body : tok_term option;
  ty : tok_term;
}

let translate_term cst_table t =
  (* n is the number of binders already in scope *)
  (* current is the next free binding index *)
  let rec aux n var_table = function
  | Var s ->
    begin try
      Lambda.Var (AssocTable.find s var_table) (* fail if the term is not closed *)
    with Not_found ->
      print_endline (s ^ " is not found.");
      raise Not_found
    end
  | Sort u -> Lambda.Sort u
  | Apply (a, b) ->
    let a = aux n var_table a in
    let b = aux n var_table b in
    Lambda.Apply (a, b)
  | Abstraction (x, a, b) ->
    let a = aux n var_table a in
    let var_table = var_table |> AssocTable.map (fun k -> k + 1)
    |> AssocTable.add x 1 in
    let b = aux (n + 1) var_table b in
    Lambda.Abstraction (a, b)
  | ProductType (x, a, b) ->
    let a = aux n var_table a in
    let var_table = var_table |> AssocTable.map (fun k -> k + 1)
    |> AssocTable.add x 1 in
    let b = aux (n + 1) var_table b in
    Lambda.ProductType (a, b)
  | Constant s -> Lambda.Constant (AssocTable.find s cst_table) (* fail if the constant is not previously defined *)
  in let t = aux 0 AssocTable.empty t
  in t

let rec show_syntax_term = function
| Var s -> s
| Sort u -> Lambda.show_term (Lambda.Sort u)
| Apply (a, b) -> show_syntax_term a ^ " " ^ show_syntax_term b
| Abstraction (x, a, b) ->
    "(\\(" ^ x ^ " : " ^ show_syntax_term a ^ ") . " ^ show_syntax_term b ^ ")"
| ProductType (x, a, b) ->
  "(/\\(" ^ x ^ " : " ^ show_syntax_term a ^ ") . " ^ show_syntax_term b ^ ")"
| Constant s -> "[" ^ s ^ "]"
let _ = show_syntax_term

let translate_def cst_table def =
  (* debugging purpose
  AssocTable.iter (fun s i -> print_endline (s ^ " : " ^ string_of_int i)) cst_table;
  let _ = Option.map (fun t -> print_endline (show_syntax_term t)) def.body in
  print_endline (show_syntax_term def.ty);
  print_newline ();*)
  let ty = translate_term cst_table def.ty
  and body = Option.map (translate_term cst_table) def.body in
  (def.name, { Lambda.body = body; Lambda.ty = ty})

module Test = struct
  let test_translate =
    let t = translate_term (AssocTable.singleton "C" 0)
      (Abstraction ("a", Constant "C", Apply (
        Abstraction ("x", Constant "C", Abstraction ("y", Constant "C", Apply (Var "y", Abstraction ("z", Constant "C", Var "z")))),
        (Abstraction ("x", Constant "C", Var "x"))
      ))) in
    assert (t = Lambda.Abstraction (Lambda.Constant 0, Lambda.Apply (
      Lambda.Abstraction (Lambda.Constant 0, Lambda.Abstraction (Lambda.Constant 0,
        Apply (Lambda.Var 1, Lambda.Abstraction (Lambda.Constant 0, Lambda.Var 1)))),
      Lambda.Abstraction (Lambda.Constant 0, Lambda.Var 1)
    )));
    let t = translate_term (AssocTable.singleton "N" 0
      |> AssocTable.add "O" 1
      |> AssocTable.add "S" 2
      |> AssocTable.add "U" 3
    ) (Abstraction ("x", Constant "N", Apply (
      Constant "S", Var "x"
    ))) in
    assert (t = Lambda.Abstraction (Lambda.Constant 0, Lambda.Apply (
      Lambda.Constant 2, Lambda.Var 1
    )))
end