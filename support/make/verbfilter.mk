############################################################-*-Makefile-*-####
# VERBFILTER - Verbfilter Latex Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make [VERBFILTER=<java-file>] <*.tex>
#
##############################################################################
# Variables
#
# VERBFILTER		 = Verb filter source file
#
##############################################################################
# Examples
#
# Generate reference.tex by verbfiltering reference.verb.tex
#
#   make VERBFILTER=../../support/latex/verbfilterScala.java reference.tex
#
##############################################################################

##############################################################################
# Defaults

JAVA			?= java
JAVAC			?= javac

##############################################################################
# Rules

%.tex			: %.verb.tex $(VERBFILTER:%.java=%.class)
	$(JAVA) -cp $(dir $(VERBFILTER)) $(notdir $(VERBFILTER:%.java=%)) $< $@

$(VERBFILTER:%.java=%.class): $(VERBFILTER)
	$(JAVAC) $?

##############################################################################
