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

LATEX_TARGETS		+= $(LATEX_FORMATS:%=rationale.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=reference.%)
LATEX_TARGETS		+= $(LATEX_FORMATS:%=examples.%)

LATEX_SOURCES		+= examples.tex
LATEX_SOURCES		+= examples.bib
LATEX_SOURCES		+= rationale.tex
LATEX_SOURCES		+= reference.tex
LATEX_SOURCES		+= rationale-chapter.tex

# latex
TEXINPUTS		:= /home/linuxsoft/share/texmf//:$(TEXINPUTS)
TEXINPUTS		:= .:$(PROJECT_SUPPORTDIR)/latex:$(TEXINPUTS):
BIBINPUTS		:= .

# verbfilter
VERBFILTER		 = $(PROJECT_SUPPORTDIR)/latex/verbfilterScala

##############################################################################
# Includes

include $(PROJECT_SUPPORTDIR)/make/verbfilter.mk
include $(PROJECT_SUPPORTDIR)/make/latex.mk

##############################################################################
