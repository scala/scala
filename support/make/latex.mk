############################################################-*-Makefile-*-####
# LATEX - Compile Latex Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make latex[.div|.ps|.pdf|.clean|.distclean]
#   make <*.[dvi|ps|pdf]>
#
##############################################################################
# Variables
#
# LATEX_TARGETS		+= list of documents to generate
# LATEX_SOURCES		+= list of documents required to generate targets
#
##############################################################################
# Examples
#
# Generate all latex-produced documents
#
#   make latex
#
#
# Generate all latex-produced PostScript documents
#
#   make latex
#
#
# Generate reference in pdf format
#
#   make reference.pdf
#
##############################################################################

##############################################################################
# Defaults

TEXINPUTS		?= .:
BIBINPUTS		?= .
LATEXMK			?= latexmk
TOUCH			?= touch

##############################################################################
# Environment

export TEXINPUTS
export BIBINPUTS

##############################################################################
# Commands

all			: latex
clean			: latex.clean
distclean		: latex.distclean

latex			: $(LATEX_TARGETS)
latex.dvi		: $(filter %.dvi,$(LATEX_TARGETS))
latex.ps		: $(filter %.ps ,$(LATEX_TARGETS))
latex.pdf		: $(filter %.pdf,$(LATEX_TARGETS))

latex.clean		:
	$(LATEXMK) -c

latex.distclean		:
	$(LATEXMK) -C

.PHONY			: latex
.PHONY			: latex.dvi
.PHONY			: latex.ps
.PHONY			: latex.pdf
.PHONY			: latex.clean
.PHONY			: latex.distclean

##############################################################################
# Rules


%.dvi			: %.tex $(LATEX_SOURCES)
	$(LATEXMK) $<

%.ps			: %.tex $(LATEX_SOURCES)
	$(LATEXMK) -ps $<

%.pdf			: %.tex $(LATEX_SOURCES)
	$(LATEXMK) -pdf $<

.PRECIOUS	: %.dvi
.PRECIOUS	: %.ps
.PRECIOUS	: %.pdf

##############################################################################
