ifeq (0,${MAKELEVEL})
top-dir   := ${PWD}
OCAMLMAKEFILE	= ${top-dir}/OCamlMakefile
MAKE := ${MAKE} top-dir=${top-dir} OCAMLMAKEFILE=${OCAMLMAKEFILE}
endif



.PHONY: ast lang

subdirs	= ast lang main

all: ${subdirs}

main: ast main
	${MAKE} -C $@ all

ast:
	${MAKE} -C $@ all

lang: ast
	${MAKE} -C $@ all

#${subdirs}:
#	${MAKE} -C $@ all

clean:
	@list='${subdirs}'; for subdir in $$list; do \
		${MAKE} -C $$subdir clean; \
	done;
