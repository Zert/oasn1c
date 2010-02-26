(*pp deriving *)

open Show;;
open Oa_asn;;

exception Eof;;
exception Error;;

let parse_channel fname channel ast_op env =
  try
    let lexbuf = Lexing.from_channel channel in
      while true do
        let result = Asn_parse.module_definition Asn_lex.token lexbuf in
		  ast_op env result
      done
  with
	  Asn_lex.Eof ->
		raise Eof
	| Oa_asn.ParsingError(expl, line, cstart, cend) ->
		begin
		  Printf.fprintf stderr "%s:%d:%d-%d error: %s\n" fname line cstart cend expl;
		  raise Error
		end
	| a ->
		begin
		  Printf.fprintf stderr "Other exception\n";
		  raise a
		end


let dump_ast ast =
  Show.show<Oa_asn.asn> ast


let ast_op env result =
  match result with
	  Main (Name (name), _tag, _extension, Body (exports, imports, assignments)) ->
		List.iter (fun assignment ->
					 let r = dump_ast assignment in
					   begin
						 Printf.printf "%s" r;
						 print_string "\n_____\n";
						 flush stdout
					   end) assignments

