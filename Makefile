# $Id: Makefile,v 1.2 2000/05/27 04:53:08 duane Exp $

SHELL = sh

default: FORCE
	@echo no default rule

clean: FORCE
	rm -f *.fasl

FORCE:
