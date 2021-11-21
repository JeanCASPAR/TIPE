  (* idx start from end *)
  let find_with_idx pred l =
    let rec aux = function
    | [] -> None, 0
    | x :: h -> let opt, n = aux h in
      match opt with
        Some (t, k) -> Some (t, k), n + 1
        | None -> if pred (n + 1) x
          then Some (x, n + 1), n+1
          else None, n+1
    in fst @@ aux l
let flip f b a = f a b
(*let idx_from_end l n = find_with_idx (fun idx _ -> idx - 1 == n) l
    |> Option.map fst*)
let rec remove_first pred = function
| h :: t -> if pred h
  then t, Some h
  else
    let (a, b) = remove_first pred t in
    h :: a, b
| [] -> [], None

type sort = Type | Kind;; (* * | ¤ *)

type expr =
  Var of var (* binded variable with his type, using de Bruijn indices *)
  | Sort of sort
  | Apply of (expr * expr) (* M N *)
  | Abstraction of (var * expr) (* (Var (x, A), m) = \x : A. m; m : * *)
  | ProductType of (var * expr) (* (Var (x, A), B) = /\x : A. B; B : ¤ *)
  | Constant of (string * expr list) (* C[Un, ..., ... U1] *)
and var = int * expr (* id * type *)

let is_sort = function
| Sort _ -> true
| _ -> false

(* remplace v par x dans e. l'appelant s'assure que x est de type snd v *)
let rec substitute (v : var) (x : expr) (e : expr) = match e with
  | Var w when fst w = fst v -> x
  | Apply (a, b) -> Apply (substitute v x a, substitute v x b)
  | Abstraction (w, f) -> Abstraction (w, substitute v x f)
  | ProductType (w, f) -> ProductType (w, substitute v x f)
  | Constant (c, l) -> Constant (c, List.map (substitute v x) l)
  | f -> f

type definition = {
  name : string;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  var : var list;
  expr : expr option; (* None is @ *)
  ty : expr;
}

type interpreter = {
  (* liste D_n :: D_(n-1) ... D_1 :: [] *)
  (* chaque D_k peut dépendre des D_l, l < k *)
  definitions : definition list;
  (* liste (x_n : A_n) :: (x_(n-1) : A_(n-1)) ... (x_1 : A_1) :: [] *)
  (* chaque A_k et x_k peut dépendre des x_l, l < k *)
  context : var list;
  (* jugement : expr * expr; (* M : N, M et N dépendent des x_k *) *)
}

(* assuming term is well-formed *)
let rec typing (machine : interpreter) = function
  | Var (_, ty) -> ty
  | Sort Type -> Sort Kind
  | Sort Kind -> failwith "not typable"
  | Apply (e1, _) -> begin match typing machine e1 with
     | Abstraction (v, e2) -> typing machine (substitute v e2 e1)
     | ProductType (v, e2) -> typing machine (substitute v e2 e1)
     | _ -> failwith "not correctly typed"
    end
  | Abstraction ((v, ty), e2) -> ProductType ((v, ty), typing machine e2)
  | ProductType _ -> Sort Type
  | Constant (c, l) ->
    (* Safe because of well-formedness of the term *)
    let def = List.find (fun def -> def.name == c) machine.definitions
    (* We use fold_right in order to substitute x1 in ty and in x2, ..., xn, etc *)
    in List.fold_right2 substitute def.var l def.ty


(* We assume term is correctly typed *)
let rec unfold (machine : interpreter) = function
| Var (v, ty) -> Var (v, unfold machine ty)
| Sort s -> Sort s
| Apply (e1, e2) -> Apply (unfold machine e1, unfold machine e2)
| Abstraction ((v, ty), e) -> Abstraction ((v, unfold machine ty), unfold machine e)
| ProductType ((v, ty), e) -> ProductType ((v, unfold machine ty), unfold machine e)
| Constant (c, l) ->
  let opt = find_with_idx (fun _ def -> def.name == c) machine.definitions
  in let l' = List.map (unfold machine) l in
  match opt with
    | None -> Constant (c, l')
    | Some (def, _) ->
      match def.expr with
        Some e -> List.fold_right2 substitute def.var l' e
        | None -> Constant (c, l')

(* vérifie qu'une expression est dérivable, pas la peine de rentrer son type,
juste de vérifier qu'il correspond bien *)
let rec check (machine : interpreter) = function
| Var (x, ty) ->
  let ctx, opt = remove_first ((==) (x, ty)) machine.context in
  let tmp_machine = match opt with
    | None -> failwith ("unknown name " ^ string_of_int x)
    | Some _ ->
      {
        definitions=machine.definitions;
        context=ctx;
      } in
  check tmp_machine ty;
  if not @@ is_sort @@ typing tmp_machine ty
  then
    failwith "a variable type should have a sort for type"
| Sort Type -> begin match machine.context with
  | [] -> () (* all checks have been done succesfully *)
  | (_, ty) :: t -> let tmp_machine = {
      definitions=machine.definitions;
      context=t;
    } in
    check tmp_machine (Sort Type); (* we check the rest of the context is well-formed *)
    check tmp_machine ty;
    if not @@ is_sort @@ typing tmp_machine ty
    then
        failwith "a variable type should have a sort for type"
  end
| Sort Kind -> failwith "not a valid term"
| Apply (e1, e2) ->
  check machine e1;
  check machine e2;
  let ty1 = typing machine e1
  and ty2 = typing machine e2 in
  begin match ty1, ty2 with
    | ProductType ((_, t), _), t' when t = t' -> ()
    | _ -> failwith "Application couldn't be typed"
  end
| Abstraction (v, e) ->
  let tmp_machine = {
    definitions=machine.definitions;
    context=v::machine.context;
  } in
  check tmp_machine e;
  let ty = typing tmp_machine e in
  check machine (ProductType (v, ty)) (* we know the type of this is a sort *)
| ProductType ((_, ty) as v, e) ->
  check machine ty;
  if not @@ is_sort @@ typing machine ty
  then
    failwith "a variable type should have a sort for type";
  let tmp_machine = {
    definitions=machine.definitions;
    context=v::machine.context;
  } in
  check tmp_machine e;
  if not @@ is_sort @@ typing tmp_machine e
  then
    failwith "a variable type should have a sort for type"
| Constant (c, l) -> (* we assume the constant is correctly defined and unrolled if needed *)
  let def = match find_with_idx (fun _ def -> def.name == c) machine.definitions with
    | None -> failwith ("unknown constant " ^ c)
    | Some (def, _) -> def
  in let id x = x in
    let f = List.fold_left2 (
    fun f (t : expr) (v : var) ->
  
      if typing machine (f t) != f (snd v)
      then failwith "provided expression has not correct type";

      fun x -> substitute v (f x) t
    ) id l def.var in
    ignore f;
    check machine (Sort Type) (* checks if the context is well-formed *)

let rec check_def (machine : interpreter) (assign : definition) =
  if List.exists (fun def -> def.name == assign.name) machine.definitions
  then failwith ("Name " ^ assign.name ^ " already used");

  let tmp_machine = List.fold_right (flip check_def) machine.definitions machine in
  ignore (check tmp_machine assign.ty);
  begin match assign.expr with
    | None -> ()
    | Some e -> ignore (check tmp_machine e);
      if unfold tmp_machine (typing tmp_machine e) != unfold tmp_machine assign.ty
      then
        failwith "Provided type do not match"
  end;
  {
    definitions = assign :: machine.definitions;
    context = machine.context
  }

let check_program = List.fold_left check_def;;

let empty = {
  definitions=[];
  context=[];
}

let show def = "name = " ^ def.name ^ ";"
