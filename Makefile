
.PHONY: ast

all: ast

ast:
	${MAKE} -C ast

clean:
	${MAKE} -C ast clean
