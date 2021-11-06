module VarAssocMap = Map.Make(String)
type syntax_error = UnknownVariable of string | Eof
exception SyntaxError of syntax_error

type tok_expr =
  Var of string
  | Sort of Lambda.sort
  | Apply of (tok_expr * tok_expr) (* M N *)
  | Abstraction of (string * tok_expr * tok_expr) (* (x, A, m) = \x : A. m; m : * *)
  | ProductType of (string * tok_expr * tok_expr) (* (x, A, B) = /\x : A. B; B : ¤ *)
  | Constant of (string * tok_expr list) (* C[Un, ..., ... U1] *)

type tok_definition = {
  name : string;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  var : (string * tok_expr) list;
  expr : tok_expr option; (* None is @ *)
  ty : tok_expr;
}

(* let rec convert (new_idx : int) (map : (int * expr) VarAssocMap.t) :
    tok_expr -> int * Lambda.Expr *)
let rec convert new_idx map = function
  | Var name -> begin match VarAssocMap.find_opt name map with
    | None -> raise (SyntaxError (UnknownVariable name))
    | Some v -> (new_idx, Lambda.Var v)
    end
  | Sort s -> (new_idx, Lambda.Sort s)
  | Apply (e1, e2) ->
    let (new_idx, f1) = convert new_idx map e1 in
    let (new_idx, f2) = convert new_idx map e2 in
    (new_idx, Lambda.Apply (f1, f2))
  | Abstraction (name, ty, e) ->
    let (new_idx, ty) = convert (new_idx) map ty in
    let id = new_idx in
    let map = VarAssocMap.add name (id, ty) map in
    let (new_idx, e) = convert (new_idx + 1) map e in
    (new_idx, Lambda.Abstraction ((id, ty), e))
  | ProductType (name, ty, e) ->
    let (new_idx, ty) = convert (new_idx) map ty in
    let id = new_idx in
    let map = VarAssocMap.add name (id, ty) map in
    let (new_idx, e) = convert (new_idx + 1) map e in
    (new_idx, Lambda.ProductType ((id, ty), e))
  | Constant (c, l) ->
    let (new_idx, l) = List.fold_left (
      fun (new_idx, t) e ->
        let (new_idx, e) = convert new_idx map e in
        (new_idx, e :: t)
    ) (new_idx, []) l in
    (new_idx, Lambda.Constant (c, l))

let convert_def new_idx def =
  let (new_idx, v, map) = List.fold_left (
    fun (new_idx, t, map) (name, ty) ->
      let (new_idx, ty) = convert new_idx map ty in
      let map = VarAssocMap.add name (new_idx, ty) map in
      (new_idx + 1, (new_idx, ty) :: t, map)
  ) (new_idx, [], VarAssocMap.empty) def.var in
  let (new_idx, ty) = convert new_idx map def.ty in
  let (new_idx, e) = match def.expr with
    | Some e ->
        let (new_idx, e) = convert new_idx map e
        in (new_idx, Some e)
    | None -> (new_idx, None) in

  (
    new_idx,
    {
      Lambda.name = def.name;
      Lambda.var = v;
      Lambda.expr = e;
      Lambda.ty = ty;
    }
  )

let convert_list_def l = snd (
  List.fold_right (
    fun def (new_idx, t) ->
      let (new_idx, def) = convert_def new_idx def
      in (new_idx, def :: t)
  ) l (0, [])
)