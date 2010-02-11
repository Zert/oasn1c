AUXLIB_DIRS	= /usr/lib/ocaml/site-packages/deriving/lib \
		${top-dir}/ast

OCAMLOPT	= ocamlopt.opt
OCAMLC		= ocamlc.opt
OCAMLDEP	= ocamldep
PATH		:= $(PATH):/usr/lib/ocaml/site-packages/deriving/syntax

OCAMLFLAGS	= -w Aef

SOURCES = oa_ocaml.ml

LIBS    = nums deriving oa_ast
RESULT  = oa_ocaml
CLIBS 	=

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

TRASH	= *~ *.cma ${RESULT}

all: byte-code

include $(OCAMLMAKEFILE)
