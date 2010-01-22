{
  open Lexing
  open Asn_parse
  exception Eof

  type error =
	| Illegal_character of char
  ;;

  exception Error of error * position;;

  let keyword_table = Hashtbl.create 10
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
		"ABSENT", ABSENT;
		"ABSTRACT-SYNTAX", ABSTRACT_SYNTAX;
		"ALL", ALL;
		"APPLICATION", APPLICATION;
		"AUTOMATIC", AUTOMATIC;
		"BEGIN", BEGIN;
		"BIT", BIT;
		"BMPString", BMPString;
		"BOOLEAN", BOOLEAN;
		"BY", BY;
		"CHARACTER", CHARACTER;
		"CHOICE", CHOICE;
		"CLASS", CLASS;
		"COMPONENT", COMPONENT;
		"COMPONENTS", COMPONENTS;
		"CONSTRAINED", CONSTRAINED;
		"CONTAINING", CONTAINING;
		"DEFAULT", DEFAULT;
		"DEFINITIONS", DEFINITIONS;
		"EMBEDDED", EMBEDDED;
		"ENCODED", ENCODED;
		"END", END;
		"ENUMERATED", ENUMERATED;
		"EXCEPT", EXCEPT;
		"EXPLICIT", EXPLICIT;
		"EXPORTS", EXPORTS;
		"EXTENSIBILITY", EXTENSIBILITY;
		"EXTERNAL", EXTERNAL;
		"FALSE", FALSE;
		"FROM", FROM;
		"GeneralizedTime", GeneralizedTime;
		"GeneralString", GeneralString;
		"GraphicString", GraphicString;
		"IA5String", IA5String;
		"IDENTIFIER", IDENTIFIER;
		"IMPLICIT", IMPLICIT;
		"IMPLIED", IMPLIED;
		"IMPORTS", IMPORTS;
		"INCLUDES", INCLUDES;
		"INSTANCE", INSTANCE;
		"INTEGER", INTEGER;
		"INTERSECTION", INTERSECTION;
		"ISO646String", ISO646String;
		"MAX", MAX;
		"MIN", MIN;
		"MINUS-INFINITY", MINUS_INFINITY;
		"NULL", NULL;
		"NumericString", NumericString;
		"OBJECT", OBJECT;
		"ObjectDescriptor", ObjectDescriptor;
		"OCTET", OCTET;
		"OF", OF;
		"OPTIONAL", OPTIONAL;
		"PATTERN", PATTERN;
		"PDV", PDV;
		"PLUS-INFINITY", PLUS_INFINITY;
		"PRESENT", PRESENT;
		"PrintableString", PrintableString;
		"PRIVATE", PRIVATE;
		"REAL", REAL;
		"RELATIVE-OID", RELATIVE_OID;
		"SEQUENCE", SEQUENCE;
		"SET", SET;
		"SIZE", SIZE;
		"STRING", STRING;
		"SYNTAX", SYNTAX;
		"T61String", T61String;
		"TAGS", TAGS;
		"TeletexString", TeletexString;
		"TRUE", TRUE;
		"TYPE-IDENTIFIER", TYPE_IDENTIFIER;
		"UNION", UNION;
		"UNIQUE", UNIQUE;
		"UNIVERSAL", UNIVERSAL;
		"UniversalString", UniversalString;
		"UTCTime", UTCTime;
		"UTF8String", UTF8String;
		"VideotexString", VideotexString;
		"VisibleString", VisibleString;
		"WITH", WITH;
 	  ]

  let db str ret =
	begin
	  Printf.printf "%s\n" str;
	  ret
	end;;

}

let uppercase = ['A'-'Z']
let lowercase = ['a'-'z']
let digit = ['0'-'9']
let hyphen = '-'
let ordsym = (uppercase|lowercase|digit)

let whitespace = ['\t' '\n' '\013' '\014' '\r' ' ']
let newline = ['\n' '\013' '\014' '\r']

let typereference = (uppercase (ordsym|hyphen)* ordsym*)
let identifier = (lowercase (ordsym|hyphen) ordsym*)
let valuereference = identifier
let modulereference = typereference
let number = ('0'|(['1'-'9']digit*))
let realnumber = (digit+ (('.'?)|('.'digit+)))
let bstring = ('\'' ('0'|'1'|' ')* '\'' 'B')
let hstring = ('\'' (['A'-'F' '0'-'9' ' ']*) '\'' 'H')
  (* add cstring *)
let cstring = ('"' ([^'"']*) '"')
let assign = "::="
let rangesep = ".."
let ellipsis = "..."
let lvbrack = "[["
let rvbrack = "]]"


  rule token = parse
	  whitespace +              { (token lexbuf) }
	| "--"                      { comment lexbuf }
	| '-'                       { db "Minus" MINUS }
	| "/*"                      { mlcomment lexbuf }
	| assign                    { db "Defn" DEFN }
	| typereference as t        { try
									db ("KW: " ^  (Lexing.lexeme lexbuf)) (Hashtbl.find keyword_table t)
								  with Not_found ->
									db ("String: " ^ (Lexing.lexeme lexbuf))  (NAME t) }
	| identifier as i           { db ("Ident: " ^ (Lexing.lexeme lexbuf)) (IDENT i) }
	| number as n               { db ("Number: " ^ (Lexing.lexeme lexbuf)) (NUMBER (int_of_string n)) }
	| realnumber as r           { db ("RealNumber: " ^ (Lexing.lexeme lexbuf)) (REALNUMBER (float_of_string r)) }
	| bstring as b              { db ("BString: " ^ (Lexing.lexeme lexbuf)) (BSTRING b) }
	| hstring as h              { db ("HString: " ^ (Lexing.lexeme lexbuf)) (HSTRING h) }
	| cstring as c              { db ("CString: " ^ (Lexing.lexeme lexbuf)) (CSTRING c) }
	| '{'                       { db "LBrace" LBRACE }
	| '}'                       { db "RBrace" RBRACE }
	| '['                       { db "LSQBrace" LSQBRACE }
	| ']'                       { db "RSQBrace" RSQBRACE }
	| ':'                       { db "Colon" COLON }
	| ';'                       { db "Semi" SEMICOLON }
	| ','                       { db "Comma" COMMA }
	| '<'                       { db "LT" LT }
	| '@'                       { db "AT" AT }
	| '*'                       { db "Star" STAR }
	| '.'                       { db "Dot" DOT }
	| '(' (digit+ as r1) rangesep (digit+ as r2) ')'   { db "Range" (RANGE ((int_of_string r1), (int_of_string r2))) }
	| '('                       { db "LParen" LPAREN }
	| ')'                       { db "RParen" RPAREN }
	| "[["                      { db "LDSQBrace" LDSQBRACE }
	| "]]"                      { db "RDSQBrace" RDSQBRACE }
	| '!'                       { db "Exclam" EXCLAM }
	| ellipsis                  { db "Dots" DOTS }
	| eof                       { raise Eof }
	| _ as r                    { Printf.printf "Rest: %c\n" r; raise Eof }
  and comment = parse
	  "--"                      { token lexbuf }
	| '\n'                      { token lexbuf }
	| _                         { comment lexbuf }
  and mlcomment = parse
	  "*/"                      { token lexbuf }
	| _                         { mlcomment lexbuf }
