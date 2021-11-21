(*open LambdaD;;
open SyntaxD;;*)

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

let check_def state {LambdaD.body=b; LambdaD.ty = ty} =
  LambdaD.show state b ty;
  let ty = LambdaD.unfold state ty in
  LambdaD.check state ty;
  Option.map (fun t ->
    let t = LambdaD.unfold state t in
    LambdaD.check state t;
    if LambdaD.typing state t != ty
    then failwith ""
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
    ) |> List.flatten |> List.map (fun ({SyntaxD.name=s} as def) ->
      print_endline s; def
    ) |> transform in
    check_defs defs;;

Printexc.record_backtrace true;;
main ();;
