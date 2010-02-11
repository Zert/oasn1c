open Oa_ast;;

let ast_op env result =
  let r = Oa_ast.dump_ast result in
    begin
	  Printf.printf "%s" r;
	  print_string "\n_____\n";
	  flush stdout
	end

let _ =
  try
	Oa_ast.parse_channel stdin ast_op []
  with Eof ->
	exit(0)

