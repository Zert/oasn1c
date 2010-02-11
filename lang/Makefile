
.PHONY: ocaml

subdirs	= ocaml

all: ocaml

ocaml:
	${MAKE} -C ocaml

clean:
	@list='${subdirs}'; for subdir in $$list; do \
		${MAKE} -C $$subdir clean; \
	done;
