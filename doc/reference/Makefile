############################################################-*-Makefile-*-####
# Scala Reference
##############################################################################
# $Id$

##############################################################################
# Configuration

ROOT			 = ../..

include $(ROOT)/Makefile.import

##############################################################################
# Variables

# project
PROJECT_SOURCES		+= $(LATEX_SOURCES)

# latex
LATEX_FORMATS		+= dvi
LATEX_FORMATS		+= ps
LATEX_FORMATS		+= pdf

LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaRationale.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaReference.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaByExample.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=ProgrammingInScala.%)

LATEX_SOURCES		+= ScalaRationale.tex
LATEX_SOURCES		+= ScalaByExample.tex
LATEX_SOURCES		+= ScalaReference.tex
LATEX_SOURCES		+= ProgrammingInScala.tex
LATEX_SOURCES		+= Scala.bib

# latex
TEXINPUTS		:= $(PROJECT_SUPPORTDIR)/latex:$(TEXINPUTS):
BIBINPUTS		:= .

##############################################################################
# Includes

include $(PROJECT_SUPPORTDIR)/make/latex.mk

##############################################################################

clean			:
	$(RM) *.out

##############################################################################
