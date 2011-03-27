AUXLIB_DIRS	= /usr/lib/ocaml/site-packages/deriving/lib
OCAMLOPT	= ocamlopt.opt
OCAMLC		= ocamlc.opt # -I +camlp4 camlp4lib.cma -pp camlp4orf
OCAMLDEP	= ocamldep
PATH		:= $(PATH):/usr/lib/ocaml/site-packages/deriving/syntax

#USE_CAMLP4	= yes

OCAMLFLAGS	= -w Aef
YFLAGS		= -v

SOURCES = oa_asn.ml \
          asn_lex.mll \
          asn_parse.mly \
          oa_ast.ml \
          oa_main.ml

LIBS    = deriving
LIB_NAME	= oa_ast
RESULT  = ${LIB_NAME}
CLIBS 	=

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

TRASH	= *~ *.cma ${RESULT}

all: quirks byte-code ${RESULT}.cma

quirks: oa_asn.cmi asn_parse.cmi

test: all test-ok test-fail

test-ok:
	(cd test && \
	../${RESULT} 1.asn && \
	../${RESULT} ACP.asn && \
	../${RESULT} megaco-v1.asn)

test-fail:
	(cd test && \
	../${RESULT} 2.asn)


include $(OCAMLMAKEFILE)


# all: oa_asn.cmo parsers rest

# rest: parsers main

# parsers: asn_lex.ml asn_parse.ml

# # clean:
# # 	rm -rf *~ *.cm* *.output asn_lex.ml asn_parse.ml asn_parse.mli oa_main

# main: quirks oa_main
# 	./oa_main < 1.asn

# quirks: parsers asn_parse.cmo

# oa_main: oa_asn.cmo asn_parse.cmo asn_lex.cmo oa_main.cmo
# 	ocamlc -pp "deriving" -o oa_main $^
# #	ocamlmktop -o st_main_top st_ast.cmo st_parse.cmo st_lex.cmo

# asn_parse.cmo: asn_parse.cmi
# 	ocamlc -c asn_parse.ml

# asn_parse.cmi: asn_parse.mli
# 	ocamlc -c asn_parse.mli

# %.ml: %.mll
# 	ocamllex $<

# %.ml: %.mly
# 	OCAMLRUNPARAM=p ocamlyacc -v $<

# %.cmo: %.ml
# 	ocamlc deriving -c $<

# %.cmi: %.mli
# 	ocamlc -c $<

