############################################################-*-Makefile-*-####
# Makefile for the Scala Compiler
##############################################################################
# $Id$

##############################################################################
# Include private stuff

-include Makefile.private

##############################################################################
# Variables

ROOT			 = .

# project
PROJECT_NAME		 = scala
PROJECT_ROOT		 = $(ROOT)
PROJECT_SOURCES		+= $(RUNTIME_SOURCES)
PROJECT_SOURCES		+= $(COMPILER_SOURCES)
PROJECT_SOURCES		+= $(INTERPRETER_SOURCES)
PROJECT_OUTPUTDIR	 = $(PROJECT_ROOT)/classes
PROJECT_CLASSPATH	 = $(PROJECT_OUTPUTDIR)
PROJECT_LISTDIR		 = $(PROJECT_ROOT)/config/list
PROJECT_SOURCEDIR	 = $(PROJECT_ROOT)/sources

# scala scripts wrapper
SCRIPTS_PREFIX		 = $(PROJECT_ROOT)/bin
SCRIPTS_WRAPPER_NAME	 = .scala_wrapper
SCRIPTS_WRAPPER_LINKS	+= $(SCRIPTS_WRAPPER_ALIASES:%=$(SCRIPTS_PREFIX)/%)
SCRIPTS_WRAPPER_ALIASES	+= socos
SCRIPTS_WRAPPER_ALIASES	+= socos-debug
SCRIPTS_WRAPPER_ALIASES	+= siris
SCRIPTS_WRAPPER_ALIASES	+= siris-debug
SCRIPTS_WRAPPER_ALIASES	+= surus
SCRIPTS_WRAPPER_ALIASES	+= surus-debug

# automatic generation of Function<n>.java and Tuple<n>.scala files
TEMPLATE_EXPANDER	 = ./bin/expand-template

FUNCTION_PREFIX		 = $(RUNTIME_ROOT)
FUNCTION_FILES		+= $(filter $(FUNCTION_PREFIX)/Function%.java,$(RUNTIME_SOURCES))
FUNCTION_TEMPLATE	 = $(FUNCTION_PREFIX)/Function.tmpl
FUNCTION_RULES		 = $(FUNCTION_PREFIX)/Function.scm

TUPLE_PREFIX		 = $(LIBRARY_ROOT)
TUPLE_FILES		+= $(filter $(TUPLE_PREFIX)/Tuple%.scala,$(LIBRARY_FILES))
TUPLE_TEMPLATE		 = $(TUPLE_PREFIX)/Tuple.tmpl
TUPLE_RULES		 = $(TUPLE_PREFIX)/Tuple.scm

# scala runtime
RUNTIME_ROOT		 = $(PROJECT_SOURCEDIR)/scala
RUNTIME_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/runtime.lst)
RUNTIME_SOURCES		+= $(RUNTIME_LIST:%=$(RUNTIME_ROOT)/%)
RUNTIME_JC_FILES	+= $(RUNTIME_SOURCES)

# scala compiler
COMPILER_ROOT		 = $(PROJECT_SOURCEDIR)/scalac
COMPILER_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/compiler.lst)
COMPILER_SOURCES	+= $(COMPILER_LIST:%=$(COMPILER_ROOT)/%)
COMPILER_JC_FILES	 = $(COMPILER_SOURCES)
COMPILER_JC_CLASSPATH	 = $(PROJECT_CLASSPATH):$(BCEL_JARFILE):$(MSIL_JARFILE)

# scala interpreter
INTERPRETER_ROOT	 = $(PROJECT_SOURCEDIR)/scalai
INTERPRETER_LIST	 = $(call READLIST,$(PROJECT_LISTDIR)/interpreter.lst)
INTERPRETER_SOURCES	+= $(INTERPRETER_LIST:%=$(INTERPRETER_ROOT)/%)
INTERPRETER_JC_FILES	 = $(INTERPRETER_SOURCES)

# scala library

LIBRARY_ROOT		 = $(RUNTIME_ROOT)
LIBRARY_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/library.lst)
LIBRARY_FILES		+= $(LIBRARY_LIST:%=$(LIBRARY_ROOT)/%)

# java compilation

JC_COMPILER		 = PICO
JC_OUTPUTDIR		 = $(PROJECT_OUTPUTDIR)
JC_CLASSPATH		 = $(PROJECT_CLASSPATH)

##############################################################################
# Variables - libraries

# BCEL (see http://jakarta.apache.org/bcel/)

BCEL_HOME		?= /home/linuxsoft/apps/BCEL-5
BCEL_JARFILE		?= $(BCEL_HOME)/bcel.jar
BCEL_LICENSE		 = $(BCEL_HOME)/LICENSE

# needed for the .NET backend

MSIL_HOME		?= /home/linuxsoft/apps/java2net
MSIL_JARFILE		?= $(MSIL_HOME)/msil.jar

##############################################################################
# Variables - tools

ECHO			?= echo
CAT			?= cat
JAR			?= jar
GREP			?= grep
FIND			?= find
WC			?= wc
SED			?= sed
M4			?= m4
RM			?= rm -f
CP			?= cp
LN			?= ln
MKDIR			?= mkdir
TOUCH			?= touch
CHMOD			?= chmod
DIRNAME			?= dirname
BASENAME		?= basename
DATE			?= date
NICE			?= nice
ZIP			?= zip
UNIX2DOS		?= unix2dos

PICO			?= pico
PICO_FLAGS		+= -make -source 1.4

##############################################################################
# Functions

RUN			 = echo $(1); $(1) || exit $$?
READLIST		 = $(shell cat $(1) | grep -v "^\#")

make			+= $(MAKE) MAKELEVEL=$(MAKELEVEL) --no-print-directory

##############################################################################
# Commands

all		: scripts
all		: runtime
all		: compiler
all		: interpreter
all		: library

force		:
	$(RM) .latest-interpreter
	$(RM) .latest-compiler
	$(RM) .latest-runtime
	@$(make) all

clean		:
	$(RM) .latest-interpreter
	$(RM) .latest-compiler
	$(RM) .latest-runtime
	$(RM) -r $(PROJECT_OUTPUTDIR)/*

distclean	: clean
	$(RM) .latest-*
	$(RM) $(TUPLE_FILES)
	$(RM) $(FUNCTION_FILES)
	$(RM) $(SCRIPTS_WRAPPER_LINKS)
	$(RM) -r $(PROJECT_OUTPUTDIR)

scripts		: $(SCRIPTS_WRAPPER_LINKS)
runtime		: .latest-runtime
compiler	: .latest-compiler
interpreter	: .latest-interpreter
library		: .latest-library

.PHONY		: all
.PHONY		: force
.PHONY		: clean
.PHONY		: distclean
.PHONY		: scripts
.PHONY		: runtime
.PHONY		: compiler
.PHONY		: interpreter
.PHONY		: library

##############################################################################
# Targets

.latest-runtime		: $(RUNTIME_JC_FILES)
	@$(MAKE) jc target=RUNTIME RUNTIME_JC_FILES='$?'
	touch $@

.latest-compiler	: $(COMPILER_JC_FILES)
	@$(make) jc target=COMPILER COMPILER_JC_FILES='$?'
	touch $@

.latest-interpreter	: $(INTERPRETER_JC_FILES)
	@$(make) jc target=INTERPRETER INTERPRETER_JC_FILES='$?'
	touch $@

.latest-library		: $(LIBRARY_FILES)
	touch $@

##############################################################################
# Rules

$(SCRIPTS_WRAPPER_LINKS):
	$(LN) -s $(SCRIPTS_WRAPPER_NAME) $@;

$(FUNCTION_FILES): $(FUNCTION_TEMPLATE) $(FUNCTION_RULES)
	$(TEMPLATE_EXPANDER) $(FUNCTION_RULES) $(FUNCTION_TEMPLATE) $@

$(TUPLE_FILES): $(TUPLE_TEMPLATE) $(TUPLE_RULES)
	$(TEMPLATE_EXPANDER) $(TUPLE_RULES) $(TUPLE_TEMPLATE) $@

##############################################################################

include $(ROOT)/support/make/jc.mk
include $(ROOT)/support/make/exec.mk
include $(ROOT)/support/make/grep.mk
include $(ROOT)/support/make/wc.mk

##############################################################################
