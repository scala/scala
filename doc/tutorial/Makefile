# Makefile to compile the Scala tutorial
# $Id$

LATEX_TARGETS = tutorial.ps tutorial.pdf

ROOT = ../..

# Thing to add to the class path to run Scala programs.
SCALA_CLASS_PATH = /home/linuxsoft/apps/scala/lib/scala.jar

# Commands
ENV = env
LATEXMK = latexmk
SCALATEX = /home/schinz/projects/scsh-scripts/scalatex/scalatex.scm

all: $(LATEX_TARGETS)

%.tex: %.scalatex
	$(ENV) CLASSPATH=$(SCALA_CLASS_PATH):$(CLASSPATH) $(SCALATEX) $< $@

include $(ROOT)/support/make/latex.mk
