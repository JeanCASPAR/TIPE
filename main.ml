let perform path f =
  let ic = open_in path in
  let res = f ic in
  close_in ic;
  res;;

let transform l =
  let rec aux cst_table i = function
  | [] -> []
  | def :: tl -> let (name, def) = Syntax.translate_def cst_table def in
    def :: aux (Syntax.AssocTable.add name i cst_table) (i + 1) tl
  in aux Syntax.AssocTable.empty 0 l;;

(* Returned the reduced definition if it is correct, raise an error else *)
let check_def state {Lambda.body = b; Lambda.ty = ty} =
  Lambda.show state b ty;
  let ty = Lambda.unfold state ty in
  Lambda.check state ty;
  let ty = Lambda.reduce ty in
  let body = Option.map (fun term ->
    let term = Lambda.unfold state term in
    Lambda.check state term;
    let real_ty = Lambda.typing state term in
    if real_ty <> ty
    then raise (Lambda.Error (Lambda.TypesDoNotMatch (real_ty, ty,
      "term x = " ^ Lambda.show_term term ^ " : " ^ Lambda.show_term real_ty ^
      " is not of the provided type " ^ Lambda.show_term ty)))
  else Lambda.reduce term
  ) b in
  {Lambda.body = body; Lambda.ty = ty}

let check_defs defs =
  let rec aux ({Lambda.defs = defs; _} as state) = function
  | def :: tl -> let def = check_def state def in
    aux ({state with Lambda.defs = Array.append defs [|def|]}) tl
  | [] -> ()
  in aux {Lambda.defs = [||]; Lambda.vars = []} defs;;


let main () =
  let defs = Array.to_list Sys.argv |> List.tl |>
    List.map (fun p ->
      print_endline p;
      try
        let def_list = perform p (fun ic ->
          let lexbuf = Lexing.from_channel ic in
          Parser.parse_file Lexer.token lexbuf
        ) in def_list
      with
        | Failure s -> print_endline ("ERROR: " ^ s); []
    ) |> List.flatten |> List.map (fun ({Syntax.name = s; _} as def) ->
      print_endline s; def
    ) |> transform in
    check_defs defs;;

Printexc.record_backtrace true;;
let catch f =
  try
    f ()
  with
    | Lambda.Error (Lambda.NotTypable (t, s)) -> print_endline s;
      print_endline (Lambda.show_term t)
    | Lambda.Error (Lambda.IsNotASort (t, ty)) -> print_endline
      (Lambda.show_term t ^ " : " ^ Lambda.show_term ty ^ " should be : s")
    | Lambda.Error (Lambda.ConstantOutOfBound k) -> print_endline
      ("Constant " ^ string_of_int k ^ " is unknown")
    | Lambda.Error (Lambda.TypesDoNotMatch (_, _, s)) -> print_endline s
    | Lambda.Error (NotClosedTerm k) -> print_endline
      ("Variable " ^ string_of_int k ^ " is free")

module Test = struct
  let n = {
    Lambda.ty = Lambda.Sort Lambda.Type;
    Lambda.body = None;
  }
  let o = {
    Lambda.ty = Lambda.Constant 0;
    Lambda.body = None;
  }
  let s = {
    Lambda.ty = Lambda.ProductType (Lambda.Constant 0, Lambda.Constant 0);
    Lambda.body = None;
  }
  let u = {
    Lambda.ty = Lambda.Constant 0;
    Lambda.body = Some (Lambda.Apply (Lambda.Constant 2, Lambda.Constant 1));
  }
  let state = {
    Lambda.vars = [];
    Lambda.defs = [|
      n; o; s
    |];
  }
  let test () = ignore (check_def state u);
    ()
  
  let test_func_type () =
    let state = {
      Lambda.defs = [||];
      Lambda.vars = [Lambda.Sort Type; Sort Type];
    } in
    let term = Lambda.ProductType (Var 2, Var 2)
    in
    let ty = Lambda.Sort Type
    in
    let def =
      {
        Lambda.body = Some term;
        Lambda.ty = ty;
      }
    in ignore (check_def state def);

    let state = {
      Lambda.defs = [||];
      Lambda.vars = [Sort Type];
    } in
    let term = Lambda.Abstraction (Sort Type,
      ProductType (Var 2, Var 2)
    ) in
    let ty = Lambda.ProductType (Sort Type, Sort Type)
    in
    let def =
      {
        Lambda.body = Some term;
        Lambda.ty = ty;
      }
    in ignore (check_def state def);


    let state = {
      Lambda.defs = [||];
      Lambda.vars = [];
    } in
    let term = Lambda.Abstraction (Sort Type,
    Abstraction (Sort Type,
      ProductType (Var 2, Var 2)
    )) in
    let ty = Lambda.ProductType (Sort Type,
      ProductType (Sort Type, Sort Type)
    ) in
    let def =
      {
        Lambda.body = Some term;
        Lambda.ty = ty;
      }
    in ignore (check_def state def);
    ()
  
  let test_id () =
    let state = {
      Lambda.defs = [||];
      vars = [Var 1; Sort Type];
    } in
    let term = Lambda.Var 1
    in
    let ty = Lambda.Var 2
    in
    let def = {
      Lambda.body = Some term;
      ty;
    }
  in ignore (check_def state def);

    let state = {
      Lambda.defs = [||];
      vars = [Sort Type];
    } in
    let term = Lambda.Abstraction (Var 1, Var 1)
    in
    let ty = Lambda.ProductType (Var 1, Var 2)
    in
    let def = {
      Lambda.body = Some term;
      ty;
    }
  in ignore (check_def state def);
    
    let state = {
      Lambda.defs = [||];
      vars = [];
    } in
    let term = Lambda.Abstraction (Sort Type,
      Lambda.Abstraction (Var 1, Var 1)
    ) in
    let ty = Lambda.ProductType (Sort Type,
      Lambda.ProductType (Var 1, Var 2)
    ) in
    let def = {
      Lambda.body = Some term;
      ty;
    }
  in ignore (check_def state def);
end;;

catch main;;
