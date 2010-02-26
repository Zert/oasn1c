open Oa_ast;;

exception Bad_lvalue;;

let ast_op env result =
  let r = Oa_ast.dump_ast result in
    begin
	  Printf.printf "%s" r;
	  print_string "\n_____\n";
	  flush stdout
	end

(* let _ = *)
(*   try *)
(* 	Oa_ast.parse_channel stdin ast_op [] *)
(*   with Eof -> *)
(* 	exit(0) *)


let _ =
  let (fname, ch) = if Array.length Sys.argv > 1 then
	(Sys.argv.(1), open_in Sys.argv.(1))
  else
	("<stdin>", stdin) in
	try
	  Oa_ast.parse_channel fname ch Oa_ast.ast_op []
	with
		Eof ->
		  exit(0)
	  | _ -> exit(1)
