############################################################-*-Makefile-*-####
# Scala Reference
##############################################################################
# $Id$

##############################################################################
# Configuration

ROOT			 = ../..

include $(ROOT)/Makefile.config

##############################################################################
# Variables

# project
PROJECT_SOURCES		+= $(LATEX_SOURCES)

# latex
LATEX_FORMATS		+= dvi
LATEX_FORMATS		+= ps
LATEX_FORMATS		+= pdf

LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaReference.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaByExample.%)

LATEX_SOURCES		+= ScalaByExample.tex
LATEX_SOURCES       += ScalaReference.tex
LATEX_SOURCES		+= Scala.bib
LATEX_SOURCES		+= Rationale.tex

# latex
TEXINPUTS		:= $(PROJECT_SUPPORTDIR)/latex:$(TEXINPUTS):
BIBINPUTS		:= .

# verbfilter
VERBFILTER		 = $(PROJECT_SUPPORTDIR)/latex/verbfilterScala

##############################################################################
# Includes

include $(PROJECT_SUPPORTDIR)/make/verbfilter.mk
include $(PROJECT_SUPPORTDIR)/make/latex.mk

##############################################################################
