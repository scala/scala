############################################################-*-Makefile-*-####
# VERBFILTER - Verbfilter Latex Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make verbfilter[.clean|.distclean]
#   make [VERBFILTER=<verbfilter-source-file>] <*.tex>
#
##############################################################################
# Variables
#
# VERBFILTER		 = Verb filter source file
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
VERBFILTER		?= $(PROJECT_SUPPORTDIR)/latex/verbfilterScala.java

##############################################################################
# Values

verbfilter		 = $(VERBFILTER:%.java=%)
verbfilter_CLASS	 = $(PROJECT_OUTPUTDIR)/$(verbfilter).class

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
	$(RM) $(verbfilter_CLASS)

.PHONY			: verbfilter
.PHONY			: verbfilter.clean
.PHONY			: verbfilter.distclean

##############################################################################
# Rules

%.tex			: %.verb.tex $(verbfilter_CLASS)
	$(JAVA) -cp $(dir $(verbfilter_CLASS)) $(notdir $(verbfilter)) $< $@

$(verbfilter_CLASS)	: $(VERBFILTER)
	$(JAVAC) $?

.PRECIOUS		: %.tex
.PRECIOUS		: $(verbfilter_CLASS)

##############################################################################
