############################################################-*-Makefile-*-####
# VERBFILTER - Verbfilter Latex Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make verbfilter[.clean|.distclean]
#   make [VERBFILTER=<verbfilter-basename>] <*.tex>
#
##############################################################################
# Variables
#
# VERBFILTER		 = Verb filter base name
#
##############################################################################
# Examples
#
# Generate all .tex files produced by verbfiltering a .verb.tex file
#
#   make verbfilter
#
#
# Generate reference.tex by verbfiltering reference.verb.tex with the
# specified verbfilter
#
#   make VERBFILTER=../../support/latex/verbfilterScala reference.tex
#
##############################################################################

##############################################################################
# Defaults

JAVA			?= java
JAVAC			?= javac

##############################################################################
# Commands

all			: verbfilter
clean			: verbfilter.clean
distclean		: verbfilter.distclean

verbfilter		: $(patsubst %.verb.tex,%.tex,$(wildcard *.verb.tex))

verbfilter.clean	:

verbfilter.distclean	:
	@for file in *.verb.tex; do \
	    [ "$$file" = "*.verb.tex" ] || ( \
	        echo $(RM) "$${file%.verb.tex}.tex"; \
	        $(RM) "$${file%.verb.tex}.tex" ); \
	done
	$(RM) $(VERBFILTER:%=%.class)

.PHONY			: verbfilter
.PHONY			: verbfilter.clean
.PHONY			: verbfilter.distclean

##############################################################################
# Rules

%.tex			: %.verb.tex $(VERBFILTER:%=%.class)
	$(JAVA) -cp $(dir $(VERBFILTER)) $(notdir $(VERBFILTER)) $< $@

$(VERBFILTER:%=%.class)	: $(VERBFILTER:%=%.java)
	$(JAVAC) $?

.PRECIOUS		: %.tex
.PRECIOUS		: $(VERBFILTER:%=%.class)

##############################################################################
