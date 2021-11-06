let perform path f =
  let ic = open_in path in
  let res = f ic in
  close_in ic;
  res;;


let main () =
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