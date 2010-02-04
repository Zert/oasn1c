(*pp deriving *)

(*
perl -ne 'if (m/\{ ([A-Z][a-zA-Z0-9]+) \}/) { print "$1\n" } elsif (m/\{ ([A-Z][a-zA-Z0-9]+) (\(.*?\)) \}/) {$l = scalar(split(",", $2)); $d = join(" * ", ("asn", "asn", "asn", "asn", "asn", "asn", "asn", "asn")[1..$l]);print "$1 of ( $d )\n"}' asn_parse.mly
 *)

type asn =
	Main of (asn * asn * asn * asn)
  | Body of (asn * asn * asn list)
  | Name of string
  | Number of int
  | AbsRef of ( string * asn )
  | All
  | AlternativeType of ( asn list )
  | AutomaticTags
  | BitBString of ( string )
  | BitHString of ( string )
  | BitString of ( string )
  | BitStringL of ( string list )
  | BitStringVal of ( asn )
  | BitStringValL of ( asn list )
  | BMPString
  | Boolean
  | BuiltinVal of ( asn )
  | CharacterString
  | CharStringList of ( asn )
  | CharSyms of ( asn list )
  | ChoiceType of ( asn )
  | ChoiceValue of ( string * asn )
  | ClassNumber of ( int )
  | ClassNumberDef of ( asn )
  | ComponentTypeListsExcept
  | ComponentTypeLists of ( asn list )
  | ComponentTypeOf of ( asn )
  | ComponentType of ( asn * asn )
  | ConstrainedType of ( asn * asn )
  | Constraint of ( asn * asn )
  | ConstraintSize of ( asn )
  | ConstraintContents of ( asn * asn )
  | CString of ( string )
  | DefaultValue of ( asn )
  | DefId of ( asn )
  | DefinedValue of ( asn )
  | DefValObjIdCompList of ( asn * asn list )
  | ElementSetSpec of ( asn )
  | Empty
  | EnumeratedType of ( asn )
  | EnumeratedValue of ( string )
  | EnumerationItemId of ( string )
  | EnumerationItemNum of ( asn )
  | Enumerations of ( asn list * asn * asn )
  | ExplicitTags
  | ExportAll
  | Exports of ( asn )
  | ExtensImpl
  | ExtensionAdditionGroup of ( int * asn list )
  | ExtensionAddition of ( asn )
  | ExtensionAdditions of ( asn list )
  | ExtensionAndException of ( asn )
  | ExtensionEndMarker
  | ExtensionMarker
  | ExternalType
  | ExternalValue of ( asn )
  | ExtTypeRef of ( asn * asn )
  | ExtValRef of ( asn * asn )
  | GeneralString
  | GlobalModuleRef of ( asn * asn )
  | GraphicString
  | IA5String
  | ImplicitTags
  | Imports of ( asn list )
  | IntegerType of ( asn )
  | IntegerValueInt of ( int )
  | IntegerValueStr of ( string )
  | ISO646String
  | ItemidCompid of ( asn * asn )
  | LowerEndVal of ( asn )
  | LowerEndValLT of ( asn )
  | Max
  | Min
  | MinusInfinity
  | ModType of ( asn * string )
  | Module of ( string )
  | NamedBitInt of ( string * int )
  | NamedBitDef of ( string * asn )
  | NamedNumberInt of ( string * int )
  | NamedNumberDef of ( string * asn )
  | NamedNumberList of ( asn list )
  | NamedTaggedType of ( string * asn )
  | NamedType of ( string * asn )
  | NamedValue of ( string * asn )
  | NameNumber of ( string * asn )
  | Null
  | NullType
  | NumericString
  | ObjAssign of ( asn )
  | ObjClassAssign of ( asn )
  | ObjClassFieldVal of ( asn )
  | ObjIdCompList of (asn list)
  | ObjIdValue of ( asn )
  | ObjSetAssign of ( asn )
  | OctBString of ( string )
  | OctetStringType of ( asn )
  | OctHString of ( string )
  | OctString of ( asn )
  | Optional
  | ParamAssign of ( asn )
  | ParameterList of ( asn * asn )
  | ParamRef of ( asn )
  | ParamTypeAssign of ( asn * asn * asn )
  | ParamType of ( asn )
  | ParamVal of ( asn )
  | ParamValSetType of ( asn )
  | ParamValueAssign of ( asn * asn * asn * asn )
  | ParamValueSetTypeAssign of ( asn * asn * asn * asn )
  | PlusInfinity
  | PrintableString
  | Quadruple of ( int * int * int * int )
  | Range of ( int * int )
  | RealType
  | Real of ( float )
  | Ref of ( string )
  | RefTypeDefined of ( asn )
  | RefTypeFromObj of ( asn )
  | RefTypeSelection of ( asn )
  | RefTypeUseful of ( asn )
  | RefTypeValSetFromObj of ( asn )
  | RefVal of ( asn )
  | SequenceOfValue of ( asn list )
  | SequenceOfConstraint of ( asn * asn )
  | SequenceType of ( asn * asn )
  | SequenceTypeExt of ( asn * asn )
  | SequenceTypeComp of ( asn * asn )
  | SequenceValue of ( asn list )
  | SetOfConstraint of ( asn * asn )
  | SetOfValue of ( asn list )
  | SetValue of ( asn list )
  | SignedNumber of ( int )
  | Size of asn
  | String of asn * asn
  | Symbols of ( asn list )
  | SymbolsFromModule of ( asn list * asn )
  | T61String
  | TagClassApplication of ( asn )
  | TagClass of ( asn )
  | TagClassPrivate of ( asn )
  | TagClassUniversal of ( asn )
  | TagExplType of ( asn * asn )
  | TagImplType of ( asn * asn )
  | TagType of ( asn * asn )
  | TeletexString
  | Tuple of ( int * int )
  | TypeAssign of ( asn * asn )
  | TypeBitString of ( asn )
  | TypeBoolean of ( asn )
  | TypeBuiltin of ( asn )
  | TypeCharString of ( asn )
  | TypeChoice of ( asn )
  | TypeConstrained of ( asn )
  | TypeEmbedPdv of ( asn )
  | TypeEnum of ( asn )
  | TypeExternal of ( asn )
  | TypeInstanceOf of ( asn )
  | TypeInteger of ( asn )
  | TypeNull of ( asn )
  | TypeObjClassField of ( asn )
  | TypeObjIdent of ( asn )
  | TypeOctetString of ( asn )
  | Type of ( string )
  | TypeReal of ( asn )
  | TypeReferenced of ( asn )
  | TypeRef of ( string )
  | TypeRelativeOid of ( asn )
  | TypeSelection of ( string * asn )
  | TypeSequence of ( asn )
  | TypeSequenceOf of ( asn )
  | TypeSet of ( asn )
  | TypeSetOf of ( asn )
  | TypeTagged of ( asn )
  | TypeUseful of ( string )
  | TypeValue of ( asn * asn )
  | UniversalString
  | UpperEndVal of ( asn )
  | UpperEndValLT of ( asn )
  | UTF8String
  | ValRef of ( asn )
  | ValueAssign of ( asn * asn * asn )
  | ValueBitString of ( asn )
  | ValueBoolean of ( bool )
  | ValueCharString of ( asn )
  | ValueChoice of ( asn )
  | ValueEmbedPdv of ( asn )
  | ValueEnum of ( asn )
  | ValueExternal of ( asn )
  | ValueInstanceOf of ( asn )
  | ValueInteger of ( asn )
  | ValueNull of ( asn )
  | ValueObjIdent of ( asn )
  | ValueOctetString of ( asn )
  | Value of ( string )
  | ValueRange of ( asn * asn )
  | ValueReal of ( asn )
  | ValueRelativeOid of ( asn )
  | ValueSequence of ( asn )
  | ValueSequenceOf of ( asn )
  | ValueSet of ( asn )
  | ValueSetOf of ( asn )
  | ValueSetTypeAssign of ( string * asn * asn )
  | ValueTagged of ( asn )
  | VideotexString
  | VisibleString
	  deriving (Show)
