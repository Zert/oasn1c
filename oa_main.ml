(*pp deriving *)

open Oa_asn;;
open Show;;

exception Bad_lvalue;;

(*
let rec stringify r =
  match r with
	  Main (s, a) ->
		Printf.sprintf "\nMain '%s': %s" s (stringify a)
	| Body (a, al) ->
		Printf.sprintf "Body:\n%s\n%s" (stringify a) (String.concat "" (List.map stringify al))
	| Imports [] -> ""
	| Imports i ->
		Printf.sprintf "Imports:\n%s" (String.concat "" (List.map stringify i))
	| ImportEntry (t, m) ->
		Printf.sprintf "** Types `%s` from %s\n" (String.concat ", " t) m
	| DefEntry (e, t) ->
		Printf.sprintf "%s :: %s\n" e (stringify t)
	| DefPlain (t, d) ->
		Printf.sprintf "%s # %s\n" t (stringify d)
	| DefSequence (t, d) ->
		Printf.sprintf "%s #\n%s\n\n" t (String.concat "" (List.map stringify d))
	| DefChoice (t, d) ->
		Printf.sprintf "%s #\n%s\n\n" t (String.concat "" (List.map stringify d))
	| DefinedType s ->
		Printf.sprintf "<%s>" s
	| Extent ->
		Printf.sprintf "@ext@"
	| Integer -> Printf.sprintf "Integer"
	| IntegerRange (r1, r2) -> Printf.sprintf "Integer (%d..%d)" r1 r2
	| _ -> Printf.sprintf "Unhandled "
 *)

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

