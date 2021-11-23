let perform path f =
  let ic = open_in path in
  let res = f ic in
  close_in ic;
  res;;

(*let main () =
  let l = List.fold_left (fun t p ->
    print_endline p;
    try
      let def_list = perform p (fun ic ->
        let lexbuf = Lexing.from_channel ic in
        Parser.parse_file Lexer.token lexbuf
      ) in def_list @ t
    with
      | Syntax.SyntaxError Eof ->
        print_endline "Error: EOF"; t
      | Syntax.SyntaxError (UnknownVariable s) ->
        print_endline ("Error: Unknown variable " ^ s); t
  ) [] (List.tl (Array.to_list Sys.argv)) in

  let l = Syntax.convert_list_def l in

  Lambda.check_program Lambda.empty l;;

Printexc.record_backtrace true;;

(*
main ();;
*)

let ic = open_in "test.tipe";;
let lexbuf = Lexing.from_channel ic;;
let def_list = Parser.parse_file Lexer.token lexbuf;;
let l = Syntax.convert_list_def def_list;;
let l = Lambda.check_program Lambda.empty l;;
*)

let transform l =
  let rec aux cst_table i = function
  | [] -> []
  | def :: tl -> let (name, def) = SyntaxD.translate_def cst_table def in
    def :: aux (SyntaxD.AssocTable.add name i cst_table) (i + 1) tl
  in aux SyntaxD.AssocTable.empty 0 l;;

let check_def state {LambdaD.body = b; LambdaD.ty = ty} =
  LambdaD.show state b ty;
  let ty = LambdaD.unfold state ty in
  LambdaD.check state ty;
  Option.map (fun term ->
    let term = LambdaD.unfold state term in
    LambdaD.check state term;
    let real_ty = LambdaD.typing state term in
    if real_ty <> ty
    then raise (LambdaD.Error (LambdaD.TypesDoNotMatch (real_ty, ty,
      "term x = " ^ LambdaD.show_term term ^ " : " ^ LambdaD.show_term real_ty ^
      " is not of the provided type " ^ LambdaD.show_term ty)))
  ) b |> ignore

let check_defs defs =
  let rec aux ({LambdaD.defs = defs} as state) = function
  | def :: tl -> check_def state def;
    aux ({state with LambdaD.defs = Array.append defs [|def|]}) tl
  | [] -> ()
  in aux {LambdaD.defs = [||]; LambdaD.vars = []} defs;;


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
    ) |> List.flatten |> List.map (fun ({SyntaxD.name = s} as def) ->
      print_endline s; def
    ) |> transform in
    check_defs defs;;

Printexc.record_backtrace true;;
let catch f =
  try
    f ()
  with
    | LambdaD.Error (LambdaD.NotTypable (t, s)) -> print_endline s;
      print_endline (LambdaD.show_term t)
    | LambdaD.Error (LambdaD.IsNotASort (t, ty)) -> print_endline
      (LambdaD.show_term t ^ " : " ^ LambdaD.show_term ty ^ " should be :  s")
    | LambdaD.Error (LambdaD.ConstantOutOfBound k) -> print_endline
      ("Constant " ^ string_of_int k ^ " is unknown")
    | LambdaD.Error (LambdaD.TypesDoNotMatch (_, _, s)) -> print_endline s
    | LambdaD.Error (NotClosedTerm k) -> print_endline
      ("Variable " ^ string_of_int k ^ " is free")

module Test = struct
  let n = {
    LambdaD.ty = LambdaD.Sort LambdaD.Type;
    LambdaD.body = None;
  }
  let o = {
    LambdaD.ty = LambdaD.Constant 0;
    LambdaD.body = None;
  }
  let s = {
    LambdaD.ty = LambdaD.ProductType (LambdaD.Constant 0, LambdaD.Constant 0);
    LambdaD.body = None;
  }
  let u = {
    LambdaD.ty = LambdaD.Constant 0;
    LambdaD.body = Some (LambdaD.Apply (LambdaD.Constant 2, LambdaD.Constant 1));
  }
  let state = {
    LambdaD.vars = [];
    LambdaD.defs = [|
      n; o; s
    |];
  }
  let test () = check_def state u
end;;

catch main;;