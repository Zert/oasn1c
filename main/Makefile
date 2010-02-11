AUXLIB_DIRS	= /usr/lib/ocaml/site-packages/deriving/lib \
		${top-dir}/ast

OCAMLOPT	= ocamlopt.opt
OCAMLC		= ocamlc.opt
OCAMLDEP	= ocamldep
PATH		:= $(PATH):/usr/lib/ocaml/site-packages/deriving/syntax

OCAMLFLAGS	= -w Aef

SOURCES = oasn1c.ml

LIBS    = nums deriving oa_ast
RESULT  = oasn1c
CLIBS 	=

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

TRASH	= *~ *.cma ${RESULT}

all: byte-code

include $(OCAMLMAKEFILE)
