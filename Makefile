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
PROJECT_SOURCES		+= $(COMPILER_SOURCES)
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

# scala compiler
COMPILER_ROOT		 = $(PROJECT_SOURCEDIR)/scalac
COMPILER_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/compiler.lst)
COMPILER_SOURCES	+= $(COMPILER_LIST:%=$(COMPILER_ROOT)/%)
COMPILER_JC_COMPILER	 = PICO
COMPILER_JC_CLASSPATH    = $(PROJECT_CLASSPATH):$(BCEL_JARFILE):$(MSIL_JARFILE)

# java compilation

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
RM			?= rm
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

JAVAC			?= javac
JAVAC_FLAGS		+=

PICO			?= pico
PICO_FLAGS		+= -make -source 1.4

##############################################################################
# Functions

RUN			 = echo $(1); $(1) || exit $$?
LOOKUP			 = $(if $($(source)_$(1)),$($(source)_$(1)),$($(1)))
READLIST		 = $(shell cat $(1) | grep -v "^\#")

make			+= $(MAKE) MAKELEVEL=$(MAKELEVEL) --no-print-directory

##############################################################################
# Commands

all		: scripts
all		: compiler

force		:
	$(RM) -f .latest-compiler
	@$(make) all

clean		:
	$(RM) -f .latest-compiler
	$(RM) -rf $(PROJECT_OUTPUTDIR)

distclean	: clean
	$(RM) -f .latest-*
	$(RM) -f $(SCRIPTS_WRAPPER_LINKS)

scripts		: $(SCRIPTS_WRAPPER_LINKS)
compiler	: .latest-compiler

.PHONY		: all
.PHONY		: force
.PHONY		: clean
.PHONY		: distclean
.PHONY		: scripts
.PHONY		: compiler

##############################################################################
# Targets

.latest-compiler	: $(COMPILER_SOURCES)
	@$(make) .do-jc source=COMPILER JC_FILES='$?'
	touch $@

##############################################################################
# Rules

$(SCRIPTS_WRAPPER_LINKS):
	$(LN) -s $(SCRIPTS_WRAPPER_NAME) $@;


##############################################################################

include $(ROOT)/support/make/exec.mk
include $(ROOT)/support/make/grep.mk
include $(ROOT)/support/make/wc.mk

##############################################################################
# JC - compile java files
##############################################################################
#
# JC_COMPILER		 = compiler name, for example JAVAC or PICO
# $(JC_COMPILER)	 = compiler command
# $(JC_COMPILER)_FLAGS	 = compiler-specific compilation flags
# JC_FLAGS		+= compilation flags
# JC_OUTPUTDIR		 = directory for the generated class files
# JC_CLASSPATH		 = class path
# JC_FILES		+= files to compile
#
##############################################################################

# setup default values

JC_COMPILER		?= JAVAC
JAVAC			?= javac

# lookup actual values

JC_COMPILER		:= $(call LOOKUP,JC_COMPILER)
JC_compiler		:= $(call LOOKUP,$(JC_COMPILER))
JC_compiler_flags	:= $(call LOOKUP,$(JC_COMPILER)_FLAGS)
JC_FLAGS		:= $(call LOOKUP,JC_FLAGS)
JC_OUTPUTDIR		:= $(call LOOKUP,JC_OUTPUTDIR)
JC_CLASSPATH		:= $(call LOOKUP,JC_CLASSPATH)
JC_FILES		:= $(call LOOKUP,JC_FILES)

# rules

.do-jc:
	@[ -d "$(JC_OUTPUTDIR)" ] || $(MKDIR) -p "$(JC_OUTPUTDIR)"
	$(strip $(JC_compiler) $(JC_compiler_flags) $(JC_FLAGS) \
	    $(JC_OUTPUTDIR:%=-d %) $(JC_CLASSPATH:%=-classpath %) \
	    $(JC_FILES))

##############################################################################
