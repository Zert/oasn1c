(*pp deriving *)

open Show;;
open Oa_asn;;

exception Bad_lvalue;;
exception Eof;;

let parse_channel channel ast_op env =
  try
    let lexbuf = Lexing.from_channel channel in
      while true do
        let result = Asn_parse.module_definition Asn_lex.token lexbuf in
		  ast_op env result
      done
  with Asn_lex.Eof ->
    raise Eof


let dump_ast ast =
  Show.show<Oa_asn.asn> ast

(*
let ast_op env result =
  match result with
	  Main (Name (name), _tag, _extension, Body (exports, imports, assignments)) ->
		depends imports
 *)
