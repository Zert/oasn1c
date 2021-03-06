%{
  open Oa_asn;;
  open Lexing;;


  let db str ret =
	begin
	  Printf.printf "%s\n" str;
	  ret
	end;;

  let parse_error s =
	Printf.fprintf stdout "\n***Error***\n";
	flush stdout;;

  let error_handler expl =
	let sp = Parsing.symbol_start_pos () in
	let ep = Parsing.symbol_end_pos () in
	  begin
		let fname = if sp.pos_fname = "" then "<stdin>" else sp.pos_fname in
		  raise (Oa_asn.ParsingError(expl, sp.pos_lnum, (sp.pos_cnum - sp.pos_bol), (ep.pos_cnum - ep.pos_bol)))
	  end

  let convert_rangelim rl =
	try
	  let i = Int64.of_string(rl) in
		Integer (i)
	with
		Failure "int_of_string" -> Value (rl)

  let convert_range r =
	match r with
		(l1, l2) -> (convert_rangelim l1, convert_rangelim l2)

%}

/*
typereference - NAME
nameform - NAME
identifier - IDENT
valuereference - IDENT
*/


%token <string> NAME
%token <string> IDENT
/*%token <string> STRING*/
%token <string> BSTRING
%token <string> HSTRING
%token <string> CSTRING
%token <int64> NUMBER
%token <float> REALNUMBER
%token <string * string> RANGERAW

/* Reserved words */
%token ABSENT ABSTRACT_SYNTAX ALL APPLICATION AUTOMATIC BEGIN BIT BMPString BOOLEAN BY
%token MINUS EXCLAM LDSQBRACE RDSQBRACE AT STAR RANGESEP
%token CHARACTER CHOICE CLASS COLON COMPONENT COMPONENTS CONSTRAINED CONTAINING DEFAULT
%token DEFINITIONS EMBEDDED ENCODED END ENUMERATED EXCEPT EXPLICIT EXPORTS EXTENSIBILITY
%token EXTERNAL FALSE FROM GeneralizedTime GeneralString GraphicString IA5String IDENTIFIER
%token IMPLICIT IMPLIED IMPORTS INCLUDES INSTANCE INTEGER INTERSECTION ISO646String MAX MIN
%token MINUS_INFINITY NULL NumericString OBJECT ObjectDescriptor OCTET OF OPTIONAL PATTERN
%token PDV PLUS_INFINITY PRESENT PrintableString PRIVATE REAL RELATIVE_OID SEQUENCE SET SIZE
%token STRING SYNTAX T61String TAGS TeletexString TRUE TYPE_IDENTIFIER UNION UNIQUE
%token UNIVERSAL UniversalString UTCTime UTF8String VideotexString VisibleString WITH


%token DEFN LBRACE RBRACE LPAREN RPAREN LSQBRACE RSQBRACE SEMICOLON COMMA DOT DOTS LT GT

%start module_definition             /* the entry point */
%type <Oa_asn.asn> module_definition
%%

module_definition:
  module_identifier DEFINITIONS tag_default extension_default DEFN BEGIN module_body END { Main ($1,$3, $4,$7) }
;
module_identifier:
  NAME                    { Name ($1) }
/* | definitive_identifier   { DefId ($1) } */
;

definitive_identifier:
  /* empty */ { Empty }
| LBRACE def_objid_comp_list RBRACE  { ObjIdCompList $2 }
;

def_objid_comp_list:
  def_objid_comp                      { [$1] }
| def_objid_comp def_objid_comp_list  { $1 :: $2 }
;
def_objid_comp:
  name_form                           { $1 }
| number_form                         { $1 }
| name_and_number_form                { $1 }
;
name_form:
  IDENT                               { Name ($1) }
| error { error_handler "Incorrect name form" }
;
number_form:
  NUMBER                              { Number ($1) }
| defined_value                       { DefinedValue ($1) }
;
name_and_number_form:
  IDENT LPAREN number_form RPAREN     { NameNumber ($1, $3) }
;

tag_default:
  /* empty */  { Empty }
| EXPLICIT TAGS           { ExplicitTags }
| IMPLICIT TAGS           { ImplicitTags }
| AUTOMATIC TAGS          { AutomaticTags }
;
extension_default:
  /* empty */ { Empty }
| EXTENSIBILITY IMPLIED        { ExtensImpl }
;
module_body:
  /* empty */           { Empty }
| exports imports assignment_list  {Body ($1, $2, $3) }
| error                     { error_handler "Incorrect body" }
;
exports:
  /* empty */                 { Exports (Empty) }
| EXPORTS symbols_exported SEMICOLON { Exports ($2) }
/*| EXPORTS ALL SEMICOLON              { ExportAll }*/
;
symbols_exported:
  ALL                             { All }
| symbol_list                     { Symbols ($1) }
;
imports:
  /* empty */            { Imports ([]) }
| IMPORTS symbols_imported SEMICOLON     { Imports ($2) }
;
symbols_imported:
  /* empty */            { [] }
| symbols_from_module_list               { $1 }
;
symbols_from_module_list:
  /* empty */                                    { [] }
| symbols_from_module                            { [$1] }
| symbols_from_module symbols_from_module_list   { $1 :: $2 }
;
symbols_from_module:
  symbol_list FROM global_module_reference       { SymbolsFromModule ($1, $3) }
;
global_module_reference:
/*  module_identifier assigned_identifier          { GlobalModuleRef ($1, $2) }*/
  module_identifier           { GlobalModuleRef ($1, Empty) }
;
assigned_identifier:
  /* empty */  { Empty }
| object_identifier_value         { ObjIdValue ($1) }
| defined_value                   { DefinedValue ($1) }
;
defined_value:
  external_value_reference        { $1 }
| value_reference                 { ValRef ($1) }
/* | parameterized_value             { ParamVal ($1) }  -- From X.683 */
;
defined_type:
  external_type_reference         { $1 }
| NAME                            { TypeRef ($1) }
/*| parameterized_type              { ParamType ($1) }
| parameterized_value_set_type    { ParamValSetType ($1) }*/
| error                           { error_handler "Incorrect type definition" }
;

/*
type_reference:
  NAME                            { TypeRef ($1) }
;
*/

object_identifier_value:
  LBRACE objid_components_list RBRACE               { ObjIdCompList ($2) }
| LBRACE defined_value objid_components_list RBRACE { DefValObjIdCompList ($2, $3) }
;
objid_components_list:
  objid_components                        { [$1] }
| objid_components objid_components_list  { $1 :: $2 }
objid_components:
  name_form            { $1 }
| number_form          { $1 }
| name_and_number_form { $1 }
| defined_value        { $1 }
;
module_reference:
  NAME                 { Module ($1) }
| error                { error_handler "Incorrect module reference format" }
;
value_reference:
  IDENT                { Value ($1) }
;
/* FIXME (?) */
reference:
  NAME                 { Ref ($1) }
;
external_type_reference:
  module_reference DOT NAME { ExtTypeRef ($1, TypeRef($3)) }
;
external_value_reference:
  module_reference DOT value_reference { ExtValRef ($1, $3) }
;
symbol_list:
  /* empty */              { [] }
| symbol                   { [$1] }
| symbol COMMA symbol_list { $1 :: $3 }
;
symbol:
  NAME              { Ref ($1) }
/* | parametrized_reference { ParamRef ($1) } */
| error                { error_handler "Incorrect symbol" }
;
assignment_list:
  assignment                 { [$1] }
| assignment assignment_list { $1 :: $2 }
;

assignment:
  type_assignment             { $1 }
| value_assignment            { $1 }
| value_set_type_assignment   { $1 }
/* | object_class_assignment     { ObjClassAssign ($1) }
| object_assignment           { ObjAssign ($1) }
| object_set_assignment       { ObjSetAssign ($1) } */
/*| parameterized_assignment    { ParamAssign ($1) }*/
| error                     { error_handler "Incorrect assignment" }
;

/* FIXME
type_assignment:
  type_reference DEFN asn_type          { TypeAssign ($1, $3) }
;
*/
type_assignment:
  NAME DEFN asn_type          { TypeAssign (TypeRef($1), $3) }
| error                       { error_handler "Bad assignment" }
;

value_assignment:
  value_reference asn_type DEFN asn_value { ValueAssign ($1, $2, $4) }
;
value_set_type_assignment:
  NAME asn_type DEFN value_set            { ValueSetTypeAssign ($1, $2, $4) }
| NAME DEFN asn_type value_set            { ValueSetTypeAssign ($1, $3, $4) }
| error                { error_handler "Incorrect value set type assignment" }
;
value_set:
  LPAREN element_set_spec RPAREN          { ValueSet ($2) }
;
element_set_spec:
  root_element_set_spec                   { ElementSetSpec ($1) }
| root_element_set_spec COMMA DOTS        { ElementSetSpec ($1) } /* Handle extending */
| root_element_set_spec COMMA DOTS COMMA
	  additional_element_set_spec         { ElementSetSpec ($1) } /* Handle extending */
;
root_element_set_spec:
  element_set_spec                        { ElementSetSpec ($1) }
;
additional_element_set_spec:
  element_set_spec                        { ElementSetSpec ($1) }
;
asn_type:
  builtin_type           { TypeBuiltin ($1) }
| referenced_type        { TypeReferenced ($1) }
| constrained_type       { TypeConstrained ($1) }
| error                  { error_handler "Not a ASN.1 type" }
;

constrained_type:
  asn_type asn_constraint  { ConstrainedType ($1, $2) }
| asn_type_with_constraint { $1 }
;
asn_type_with_constraint:
  SET asn_constraint OF asn_type           { SetOfConstraint ($2, $4) }
| SET size_constraint OF asn_type          { SetOfConstraint ($2, $4) }
| SET OF asn_type                          { SetOfConstraint (Empty, $3) }
| SET OF asn_type                          { SetOfConstraint (Empty, $3) }
| SEQUENCE asn_constraint OF asn_type      { SequenceOfConstraint ($2, $4) }
| SEQUENCE size_constraint OF asn_type     { SequenceOfConstraint ($2, $4) }
| SEQUENCE OF asn_type                     { SequenceOfConstraint (Empty, $3) }
;
asn_constraint:
  LPAREN constraint_spec RPAREN                   { Constraint ($2, Empty) }
| LPAREN constraint_spec exception_spec RPAREN    { Constraint ($2, $3) }
;
constraint_spec:
  subtype_constraint           { $1 }
| size_constraint              { $1 } /* FIXME */
| general_constraint           { $1 }
;
subtype_constraint:
  element_set_spec { $1 }
;
size_constraint:
/*  SIZE asn_constraint         { ConstraintSize ($2) } */
| SIZE value_range            { ConstraintSize ($2) }
;

/*value_range:
  lower_endpoint RANGESEP upper_endpoint   { ValueRange ($1, $3) }
;*/
value_range:
 RANGE         { $1 }
;
lower_endpoint:
  lower_end_value          { LowerEndVal ($1) }
| lower_end_value LT       { LowerEndValLT ($1) }
;
upper_endpoint:
  upper_end_value          { UpperEndVal ($1) }
| upper_end_value LT       { UpperEndValLT ($1) }
;
lower_end_value:
  asn_value                { $1 }
| MIN                      { Min }
;
upper_end_value:
  asn_value                { $1 }
| MAX                      { Max }
;


/*
subtype_elements:
  single_value
| contained_subtype
| value_range
| permitted_alphabet
| size_constraint
| type_constraint
| inner_type_constraints
| pattern_Constraint
;
elements:
  subtype_elements
| object_set_elements
| LPAREN element_spec_set RPAREN
;
*/

general_constraint:
/*  user_defined_constraint     { $1 } */
/*| table_constraint*/
| contents_constraint         { $1 }
;
/*
Governer described in X.683

user_defined_constraint:
  CONSTRAINED BY LBRACE user_defined_constraint_parameter_list RBRACE       { ConstrainedBy ($4) }
;
user_defined_constraint_parameter_list:
  / empty /                                  { [] }
| user_defined_constraint_parameter            { [$1] }
| user_defined_constraint_parameter COMMA
	  user_defined_constraint_parameter_list   { $1 :: $3 }
;
user_defined_constraint_parameter:
  governer COLON asn_value        { UserDefinedConstraint ($1, $3) }
| governer COLON value_set        { UserDefinedConstraint ($1, $3) }
| governer COLON asn_object       { UserDefinedConstraint ($1, $3) }
| governer COLON object_set       { UserDefinedConstraint ($1, $3) }
| asn_type                        { $1 }
| defined_object_class            { $1 }
;
*/
/*
X.682, Page 9
table_constraint:
  simple_table_constraint
| component_relation_constraint
;
simple_table_constraint:
  object_set
;
component_relation_constraint:
  LBRACE defined_ibject_set RBRACE LBRACE at_notation_list RBRACE
;
at_notation_list:
  at_notation                          { [$1] }
| at_notation COMMA at_notation_list   { $1 :: $3 }
;
at_notation:
  AT component_id_list
| AT DOT level component_id_list
;
level:
  { Empty }
| DOT level      / some fuckup /
;
component_id_list:
  component_id                         { [$1] }
| component_id DOT component_id_list   { $1 :: $3 }
;
*/
contents_constraint:
  CONTAINING asn_type                           { ConstraintContents ($2, Empty) }
| ENCODED BY asn_value                          { ConstraintContents (Empty, $3) }
| CONTAINING asn_type ENCODED BY asn_value      { ConstraintContents ($2, $5) }
;


referenced_type:
  defined_type             { RefTypeDefined ($1) }
| NAME                     { RefTypeUseful (TypeUseful($1)) }
| selection_type           { RefTypeSelection ($1) }
/* | type_from_object         { RefTypeFromObj ($1) } */
/* | value_set_from_objects   { RefTypeValSetFromObj ($1) } */
| error                { error_handler "Incorrect referenced type" }
;
/*
useful_type:
  NAME                     { TypeUseful ($1) }
;
*/


selection_type:
  IDENT LT asn_type        { TypeSelection ($1, $3) }
;


builtin_type:
  bit_string_type           { TypeBitString ($1) }
| boolean_type              { TypeBoolean ($1) }
| character_string_type     { TypeCharString ($1) }
| choice_type               { TypeChoice ($1) }
/*| embedded_pdv_type         { TypeEmbedPdv ($1) } */
| enumerated_type           { TypeEnum ($1) }
| external_type             { TypeExternal ($1) }
/*| instance_of_type          { TypeInstanceOf ($1) }*/
| integer_type              { TypeInteger ($1) }
| null_type                 { TypeNull ($1) }
/*| object_class_field_type   { TypeObjClassField ($1) }*/
| object_identifier_type    { TypeObjIdent ($1) }
| octet_string_type         { TypeOctetString ($1) }
| real_type                 { TypeReal ($1) }
/*| relative_oid_type         { TypeRelativeOid ($1) }*/
| sequence_type             { TypeSequence ($1) }
/*| sequence_of_type          { TypeSequenceOf ($1) }
| set_type                  { TypeSet ($1) }
| set_of_type               { TypeSetOf ($1) }
| tagged_type               { TypeTagged ($1) }*/
| error { error_handler "Bad type definition" }
;
bit_string_type:
  BIT STRING                { BitStringValL ([]) }
| BIT STRING LBRACE named_bit_list RBRACE { BitStringValL ($4) }
;
named_bit_list:
  named_bit                       { [$1] }
| named_bit COMMA named_bit_list  { $1 :: $3 }
;
named_bit:
  IDENT LPAREN NUMBER RPAREN        { NamedBitInt ($1, $3) }
| IDENT LPAREN defined_value RPAREN { NamedBitDef ($1, $3) }
;
boolean_type:
  BOOLEAN                   { Boolean }
;
character_string_type:
  restricted_character_string_type    { String ($1, Empty) }
| restricted_character_string_type LPAREN SIZE RANGE RPAREN    { String ($1, Size ($4)) }
| restricted_character_string_type LPAREN SIZE LPAREN NUMBER RPAREN RPAREN    { String ($1, Size (Integer($5))) }
| unrestricted_character_string_type  { String ($1, Empty) }
| unrestricted_character_string_type LPAREN SIZE RANGE RPAREN    { String ($1, Size ($4)) }
| unrestricted_character_string_type LPAREN SIZE LPAREN NUMBER RPAREN RPAREN    { String ($1, Size (Integer($5))) }
;
restricted_character_string_type:
  BMPString              { BMPString }
| GeneralString          { GeneralString }
| GraphicString          { GraphicString }
| IA5String              { IA5String }
| ISO646String           { ISO646String }
| NumericString          { NumericString }
| PrintableString        { PrintableString }
| TeletexString          { TeletexString }
| T61String              { T61String }
| UniversalString        { UniversalString }
| UTF8String             { UTF8String }
| VideotexString         { VideotexString }
| VisibleString          { VisibleString }
;
unrestricted_character_string_type:
  CHARACTER STRING       { CharacterString }
;
choice_type:
  CHOICE LBRACE alternative_type_lists RBRACE { ChoiceType ($3) }
;
alternative_type_lists:
  /* empty */                              { Empty }
| alternative_type_list                    { AlternativeType ($1) }
/*| alternative_type_list COMMA
	  extension_and_exception
	  extension_addition_alternatives
	  optional_extension_marker */ /* ITU-T Rec. X.680 (07/2002) Page 54 */
;
alternative_type_list:
  alternative_type                                { [$1] }
| alternative_type COMMA alternative_type_list    { $1 :: $3 }
;
alternative_type:
  named_type                { $1 }
| named_tagged_type         { $1 }
| DOTS                      { ExtensionMarker } /* FIXME */
;
enumerated_type:
  ENUMERATED LBRACE enumerations RBRACE     { EnumeratedType ($3) }
;
enumerations:
  enumeration                                { Enumerations ($1, Empty, Empty) }
| enumeration COMMA DOTS                     { Enumerations ($1, ExtensionMarker, Empty) }
| enumeration COMMA DOTS exception_spec      { Enumerations ($1, $4, Empty) }
/*| enumeration COMMA DOTS exception_spec
	  COMMA enumeration                      { Enumerations ($1, $4, $6) }*/
;
enumeration:
  enumeration_item                     { [$1] }
| enumeration_item COMMA enumeration   { $1 :: $3 }
;
enumeration_item:
  IDENT                 { EnumerationItemId ($1) }
| named_number          { EnumerationItemNum ($1) }
| DOTS                  { ExtensionMarker }
;
external_type:
  EXTERNAL              { ExternalType }
;
integer_type:
  INTEGER                                  { IntegerType (Empty) }
| INTEGER RANGE                            { IntegerType ($2) }
| INTEGER LBRACE named_number_list RBRACE  { IntegerType (NamedNumberList($3)) }
;
named_number_list:
  named_number                               { [$1] }
| named_number COMMA named_number_list       { $1 :: $3 }
;
named_number:
  IDENT LPAREN signed_number RPAREN           { NamedNumberInt ($1, $3) }
| IDENT LPAREN defined_value RPAREN           { NamedNumberDef ($1, $3) }
;
signed_number:
  NUMBER                 { $1 }
| MINUS NUMBER           { Int64.neg($2) }
null_type:
  NULL                   { NullType }
;
octet_string_type:
  OCTET STRING                           { OctetStringType (Empty) }
| OCTET STRING LPAREN SIZE RANGE RPAREN  { OctetStringType (Size ($5)) }
| OCTET STRING LPAREN SIZE LPAREN NUMBER RPAREN RPAREN  { OctetStringType (Size (Integer($6))) }
| OCTET STRING LPAREN SIZE LPAREN IDENT RPAREN RPAREN  { OctetStringType (Size (Value($6))) }
;
real_type:
  REAL                   { RealType }
;
/*sequence_type:
  SEQUENCE LBRACE RBRACE                       { SequenceType (Empty, Empty) }
| SEQUENCE LBRACE component_type_lists RBRACE  { SequenceTypeComp ($3, Empty) }
| SEQUENCE LBRACE extension_and_exception
	  optional_extension_marker RBRACE         { SequenceTypeExt ($3, $4) }
| error                                        { error_handler "Incorrect SEQUENCE type definition" }
;*/
sequence_type:
  SEQUENCE LBRACE sequence_definition RBRACE   { SequenceType ( $3 ) }
| error                                        { error_handler "Incorrect SEQUENCE type" }
;
sequence_definition:
  /* empty */                                        { (Empty, Empty) }
| component_type_lists                               { ($1, Empty) }
| extension_and_exception optional_extension_marker  { ($1, $2) }
| error                                              { error_handler "Incorrect SEQUENCE type definition" }

object_identifier_type:
  OBJECT IDENTIFIER               { Empty } /* FIXME */
;

component_type_lists:
  component_type_list                                       { ComponentTypeLists ($1) }
| component_type_list COMMA extension_and_exception
	  extension_additions optional_extension_marker         { ComponentTypeListsExcept }
| component_type_list COMMA extension_and_exception
	  extension_additions extension_end_marker
	  COMMA component_type_list                             { ComponentTypeListsExcept }
| extension_and_exception extension_additions
	  extension_end_marker COMMA component_type_list        { ComponentTypeListsExcept }
| extension_and_exception extension_additions
	  optional_extension_marker                             { ComponentTypeListsExcept }
;

extension_end_marker: COMMA DOTS  { ExtensionEndMarker }
;
extension_and_exception:
  DOTS                            { ExtensionAndException (Empty) }
| DOTS exception_spec             { ExtensionAndException ($2) }
;
exception_spec:                   { Empty }
| EXCLAM exception_identification { $2 }
;
exception_identification:
  signed_number                   { SignedNumber ($1) }
| defined_value                   { DefinedValue ($1) }
| asn_type COLON asn_value        { TypeValue ($1, $3) }
;
extension_additions:              { Empty }
| COMMA extension_addition_list   { ExtensionAdditions ($2) }
;
extension_addition_list:
  extension_addition                                { [$1] }
| extension_addition COMMA extension_addition_list  { $1 :: $3 }
;
extension_addition:
  component_type                                    { ExtensionAddition ($1) }
| extension_addition_group                          { ExtensionAddition ($1) }
;
extension_addition_group:
  LDSQBRACE version_number component_type_list RDSQBRACE  { ExtensionAdditionGroup ($2, $3) }
;
optional_extension_marker:      { Empty }
| COMMA DOTS                    { ExtensionMarker }
version_number: { (0L) }
| NUMBER COLON  { $1 }
;
component_type_list:
  component_type                           { [$1] }
| component_type COMMA component_type_list { $1 :: $3 }
;
component_type:
  named_type                             { ComponentType ($1, Empty) }
| named_type OPTIONAL                    { ComponentType ($1, Optional) }
| named_type DEFAULT asn_value           { ComponentType ($1, DefaultValue ($3)) }
| COMPONENTS OF asn_type                 { ComponentTypeOf ($3) }
| named_tagged_type                      { ComponentType ($1, Empty) }
| named_tagged_type OPTIONAL             { ComponentType ($1, Optional) }
| named_tagged_type DEFAULT asn_value    { ComponentType ($1, DefaultValue ($3)) }
| DOTS                                   { ExtensionMarker } /* FIXME */
;

named_type:
  IDENT asn_type            { NamedType ($1, $2) }
| error                     { error_handler "Bad named type" }
;
named_tagged_type:
  IDENT tagged_type         { NamedTaggedType ($1, $2) }
| error                     { error_handler "Bad named tagged type" }
;

asn_value:
  builtin_value             { BuiltinVal ($1) }
| referenced_value          { RefVal ($1) }
/*| object_class_field_value  { ObjClassFieldVal ($1) }*/
;

builtin_value:
  bit_string_value          { ValueBitString ($1) }
| boolean_value             { ValueBoolean ($1) }
| character_string_value    { ValueCharString ($1) }
| choice_value              { ValueChoice ($1) }
/*| embedded_pdv_value        { ValueEmbedPdv ($1) }*/
| enumerated_value          { ValueEnum ($1) }
| external_value            { ValueExternal ($1) }
/*| instance_of_value         { ValueInstanceOf ($1) }*/
| integer_value             { ValueInteger ($1) }
| null_value                { ValueNull ($1) }
| object_identifier_value   { ValueObjIdent ($1) }
| octet_string_value        { ValueOctetString ($1) }
| real_value                { ValueReal ($1) }
/*| relative_oid_value        { ValueRelativeOid ($1) }*/
| sequence_value            { ValueSequence ($1) }
| sequence_of_value         { ValueSequenceOf ($1) }
| set_value                 { ValueSet ($1) }
| set_of_value              { ValueSetOf ($1) }
| tagged_value              { ValueTagged ($1) }
;
referenced_value:
  defined_value  { $1 }
/*| value_from_object { $1 }*/
;
absolute_reference:
  AT NAME DOT item_spec       { AbsRef ($2, $4) }
;
item_spec:
  NAME                        { Type ($1) }
| item_id DOT component_id    { ItemidCompid ($1, $3) }
;
item_id:
  item_spec { $1 }
;
component_id:
  IDENT         { Name ($1) }
| NUMBER        { Number ($1) }
| STAR          { All }
;
tagged_type:
  tag asn_type           { TagType ($1, $2) }
| tag IMPLICIT asn_type  { TagImplType ($1, $3) }
| tag EXPLICIT asn_type  { TagExplType ($1, $3) }
;
tag:
  LSQBRACE class_number RSQBRACE             { TagClass ($2) }
| LSQBRACE UNIVERSAL class_number RSQBRACE   { TagClassUniversal ($3) }
| LSQBRACE APPLICATION class_number RSQBRACE { TagClassApplication ($3) }
| LSQBRACE PRIVATE class_number RSQBRACE     { TagClassPrivate ($3) }
;
tagged_value:
  asn_value    { $1 }
;
/*parameterized_assignment:
  parameterized_type_assignment           { $1 }
| parameterized_value_assignment          { $1 }
| parameterized_value_set_type_assignment { $1 }
| parameterized_object_class_assignment   { $1 }
| parameterized_object_assignment         { $1 }
| parameterized_object_set_assignment     { $1 }
;*/
/*parameterized_type_assignment:
  NAME parameter_list DEFN asn_type             { ParamTypeAssign ($1, $2, $4) }
;
parameterized_value_assignment:
  IDENT parameter_list asn_type DEFN asn_value  { ParamValueAssign ($1, $2, $3, $5) }
;
parameterized_value_set_type_assignment:
  NAME parameter_list asn_type DEFN value_set  { ParamValueSetTypeAssign ($1, $2, $3, $5) }
;*/
bit_string_value:
  BSTRING                           { BitBString ($1) }
| HSTRING                           { BitHString ($1) }
| LBRACE identifier_list RBRACE     { BitStringL ($2) }
| LBRACE RBRACE                     { BitStringL ([]) }
| CONTAINING asn_value              { BitStringVal ($2) }
;
identifier_list:
  IDENT                             { [$1] }
| IDENT COMMA identifier_list       { $1 :: $3 }
;
boolean_value:
  TRUE                              { true }
| FALSE                             { false }
;
character_string_value:
  restricted_character_string_value      { $1 }
| unrestricted_character_string_value    { $1 }
;
restricted_character_string_value:
  CSTRING                                { CString ($1) }
| character_string_list                  { CharStringList ($1) }
| quadruple                              { $1 }
| tuple                                  { $1 }
;
unrestricted_character_string_value:
  sequence_value                         { $1 }
;
character_string_list:
  LBRACE char_syms RBRACE                { CharSyms ($2) }
;
char_syms:
  chars_defn                    { [$1] }
| chars_defn COMMA char_syms    { $1 :: $3 }
;
chars_defn:
  CSTRING                       { CString ($1) }
| quadruple                     { $1 }
| tuple                         { $1 }
| defined_value                 { $1 }
;
quadruple:
  LBRACE group COMMA plane COMMA row COMMA cell RBRACE  { Quadruple ($2, $4, $6, $8) }
;
group:
  NUMBER    { $1 }
;
plane:
  NUMBER    { $1 }
;
row:
  NUMBER    { $1 }
;
cell:
  NUMBER    { $1 }
;
tuple:
  LBRACE table_column COMMA table_row RBRACE     { Tuple ($2, $4) }
;
table_column:
  NUMBER    { $1 }
;
table_row:
 NUMBER    { $1 }
;
choice_value:
  IDENT COLON asn_value  { ChoiceValue ($1, $3) }
;
enumerated_value:
  IDENT                  { EnumeratedValue ($1) }
;
external_value:
  sequence_value         { ExternalValue ($1) }
;
integer_value:
  signed_number          { IntegerValueInt ($1) }
| IDENT                  { IntegerValueStr ($1) }
;
null_value:
  NULL                   { Null }
;
octet_string_value:
  BSTRING                 { OctBString ($1) }
| HSTRING                 { OctHString ($1) }
| CONTAINING asn_value    { OctString ($2) }
;
real_value:
  numeric_real_value      { $1 }
| special_real_value      { $1 }
;
numeric_real_value:
  REALNUMBER              { Real ($1) }
| MINUS REALNUMBER        { Real (-. ($2)) }
| sequence_value          { $1 }
;
special_real_value:
  PLUS_INFINITY           { PlusInfinity }
| MINUS_INFINITY          { MinusInfinity }
;
sequence_value:
  LBRACE component_value_list RBRACE           { SequenceValue ($2) }
| LBRACE RBRACE                                { SequenceValue ([]) }
;
component_value_list:
  named_value                                 { [$1] }
| named_value COMMA component_value_list      { $1 :: $3 }
;
sequence_of_value:
  LBRACE value_list RBRACE                 { SequenceOfValue ($2) }
| LBRACE named_value_list RBRACE           { SequenceOfValue ($2) }
| LBRACE RBRACE                            { SequenceOfValue ([]) }
;
value_list:
  asn_value                              { [$1] }
| asn_value COMMA value_list             { $1 :: $3 }
;
named_value_list:
  named_value                            { [$1] }
| named_value COMMA named_value_list     { $1 :: $3 }
;
set_value:
  LBRACE component_value_list RBRACE       { SetValue ($2) }
| LBRACE RBRACE                            { SetValue ([]) }
;
set_of_value:
  LBRACE value_list RBRACE                 { SetOfValue ($2) }
| LBRACE named_value_list RBRACE           { SetOfValue ($2) }
| LBRACE RBRACE                            { SetOfValue ([]) }
;
class_number:
  NUMBER                                 { ClassNumber ($1) }
| defined_value                          { ClassNumberDef ($1) }
;
/*
parameter_list:
  SET SIZE RANGE OF parameter            { ParameterList ($3, $5) }
;*/
named_value:
  IDENT asn_value                        { NamedValue ($1, $2) }
;
RANGE:
  LPAREN RANGERAW RPAREN     { Range (convert_range $2) }
| LPAREN IDENT RANGESEP IDENT RPAREN      { Range (Value ($2), Value ($4)) }
| LPAREN IDENT RANGESEP NUMBER RPAREN     { Range (Value ($2), Integer ($4)) }
| LPAREN NUMBER RANGESEP IDENT RPAREN     { Range (Integer ($2), Value ($4)) }
| LPAREN NUMBER RANGESEP NUMBER RPAREN    { Range (Integer ($2), Integer ($4)) }
;

