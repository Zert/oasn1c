(*pp deriving *)

open Oa_asn;;
open Show;;

exception Bad_lvalue;;

let ast_op result =
  match result with
	  x -> "null"


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
        let result = Asn_parse.module_definition Asn_lex.token lexbuf in
		let r = Show.show<Oa_asn.asn> result in
          begin
			Printf.printf "%s" r;
			print_string "\n_____\n";
			flush stdout
		  end
      done
  with Asn_lex.Eof ->
    exit 0

