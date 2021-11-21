module AssocTable = Map.Make(String)

type tok_term =
  | Var of string
  | Universe of LambdaD.universe
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
  | Var s -> LambdaD.Var (AssocTable.find s var_table) (* fail if the term is not closed *)
  | Universe u -> LambdaD.Universe u
  | Apply (a, b) ->
    let a = aux n var_table a in
    let b = aux n var_table b in
    LambdaD.Apply (a, b)
  | Abstraction (x, a, b) ->
    let a = aux n var_table a in
    let var_table = var_table |> AssocTable.map (fun k -> k + 1)
    |> AssocTable.add x 1 in
    let b = aux (n + 1) var_table b in
    LambdaD.Abstraction (a, b)
  | ProductType (x, a, b) ->
    let a = aux n var_table a in
    let var_table = var_table |> AssocTable.map (fun k -> k + 1)
    |> AssocTable.add x 1 in
    let b = aux (n + 1) var_table b in
    LambdaD.ProductType (a, b)
  | Constant s -> LambdaD.Constant (AssocTable.find s cst_table) (* fail if the constant is not previously defined *)
  in let t = aux 0 AssocTable.empty t
  in t

let translate_def cst_table def =
  let ty = translate_term cst_table def.ty
  and body = Option.map (translate_term cst_table) def.body in
  (def.name, { LambdaD.body = body; LambdaD.ty = ty})

module Test = struct
  let test_translate () =
    let t = translate_term (AssocTable.singleton "C" 0)
      (Abstraction ("z", Constant "C", (Apply (
        Abstraction ("x", Constant "C", Abstraction ("y", Constant "C", Apply (Var "y", Abstraction ("z", Constant "C", Var "z")))),
        (Abstraction ("x", Constant "C", Var "x"))
      )))) in
    assert (t = LambdaD.Abstraction (LambdaD.Constant 0, LambdaD.Apply (
      LambdaD.Abstraction (LambdaD.Constant 0, LambdaD.Abstraction (LambdaD.Constant 0,
        Apply (LambdaD.Var 1, LambdaD.Abstraction (LambdaD.Constant 0, LambdaD.Var 1)))),
      LambdaD.Abstraction (LambdaD.Constant 0, Apply (LambdaD.Var 2, LambdaD.Var 1))
    )))
end
