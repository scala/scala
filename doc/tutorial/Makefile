############################################################-*-Makefile-*-####
# Scala Tutorial
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

LATEX_TARGETS		+= $(LATEX_FORMATS:%=tutorial.%)

LATEX_SOURCES		+= tutorial.tex

##############################################################################
# Includes

include $(PROJECT_SUPPORTDIR)/make/scalatex.mk
include $(PROJECT_SUPPORTDIR)/make/latex.mk

##############################################################################
