############################################################-*-Makefile-*-####
# Scala Tutorial
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

LATEX_TARGETS		+= $(LATEX_FORMATS:%=ScalaTutorial.%)

LATEX_SOURCES		+= ScalaTutorial.tex

# latex
TEXINPUTS		:= $(PROJECT_SUPPORTDIR)/latex:$(TEXINPUTS):

##############################################################################
# Includes

include $(PROJECT_SUPPORTDIR)/make/scalatex.mk
include $(PROJECT_SUPPORTDIR)/make/latex.mk

##############################################################################
