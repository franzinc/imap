
SHELL = sh

default: FORCE
	@echo no default rule

clean: FORCE
	rm -f *.fasl

FORCE:
