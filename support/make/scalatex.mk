############################################################-*-Makefile-*-####
# SCALATEX - Evaluate Embedded Scala Scripts
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make scalatex[.clean|.distclean]
#   make <*.tex>
#
##############################################################################
# Examples
#
# Generate all .tex files produced by processing a .scala.tex file
#
#   make scalatex
#
#
# Generate tutorial.tex by processing tutorial.scala.tex
#
#   make tutorial.tex
#
##############################################################################

##############################################################################
# Defaults

SCSH			?= scsh

##############################################################################
# Values

scalatex_ENV		?= CLASSPATH=$(PROJECT_CLASSPATH)
scalatex_SCRIPT		?= $(PROJECT_SUPPORTDIR)/latex/scalatex.scm

##############################################################################
# Commands

all			: scalatex
clean			: scalatex.clean
distclean		: scalatex.distclean

scalatex		: $(patsubst %.scala.tex,%.tex,$(wildcard *.scala.tex))

scalatex.clean		:

scalatex.distclean	:
	@for file in *.scala.tex; do \
	    [ "$$file" = "*.scala.tex" ] || ( \
	        echo $(RM) "$${file%.scala.tex}.tex"; \
	        $(RM) "$${file%.scala.tex}.tex" ); \
	done

.PHONY			: scalatex
.PHONY			: scalatex.clean
.PHONY			: scalatex.distclean

##############################################################################
# Rules

%.tex			: %.scala.tex
	$(scalatex_ENV) $(SCSH) -e main -s $(scalatex_SCRIPT) $< $@

.PRECIOUS		: %.tex

##############################################################################
