############################################################-*-Makefile-*-####
# Makefile for the Scala Compiler
##############################################################################
# $Id$

##############################################################################
# Configuration

ROOT			 = .

include $(ROOT)/Makefile.config

##############################################################################
# Variables

# project
PROJECT_SOURCES		+= $(LAMPLIB_SOURCES)
PROJECT_SOURCES		+= $(META_SOURCES)
PROJECT_SOURCES		+= $(RUNTIME_SOURCES)
PROJECT_SOURCES		+= $(COMPILER_SOURCES)
PROJECT_SOURCES		+= $(INTERPRETER_SOURCES)
PROJECT_SOURCES		+= $(SCALADOC_SOURCES)
PROJECT_SOURCES		+= $(DTD2SCALA_SOURCES)

# project java archive
PROJECT_JAR_ARCHIVE	 = $(ROOT)/lib/$(PROJECT_NAME).jar
PROJECT_JAR_MANIFEST	 = $(PROJECT_SOURCEDIR)/MANIFEST
PROJECT_JAR_INPUTDIR	 = $(PROJECT_OUTPUTDIR)
PROJECT_JAR_FILES	+= ch
PROJECT_JAR_FILES	+= scala
PROJECT_JAR_FILES	+= scalac
PROJECT_JAR_FILES	+= scalai

# scala scripts wrapper
SCRIPTS_PREFIX		 = $(PROJECT_ROOT)/bin
SCRIPTS_WRAPPER		 = $(SCRIPTS_PREFIX)/.scala_wrapper
SCRIPTS_WRAPPER_LINKS	+= $(SCRIPTS_WRAPPER_ALIASES:%=$(SCRIPTS_PREFIX)/%)
SCRIPTS_WRAPPER_ALIASES	+= scala-info
SCRIPTS_WRAPPER_ALIASES	+= socos
SCRIPTS_WRAPPER_ALIASES	+= socos-debug
SCRIPTS_WRAPPER_ALIASES	+= siris
SCRIPTS_WRAPPER_ALIASES	+= siris-debug
SCRIPTS_WRAPPER_ALIASES	+= surus
SCRIPTS_WRAPPER_ALIASES	+= surus-debug
SCRIPTS_WRAPPER_ALIASES	+= dtd2scala

# automatic generation of Function<n>.java and Tuple<n>.scala files
FUNCTION_PREFIX		 = $(RUNTIME_ROOT)
FUNCTION_FILES		+= $(filter $(FUNCTION_PREFIX)/Function%.java,$(RUNTIME_SOURCES))
FUNCTION_TEMPLATE	 = $(FUNCTION_PREFIX)/Function.java.tmpl

TUPLE_PREFIX		 = $(LIBRARY_ROOT)
TUPLE_FILES		+= $(filter $(TUPLE_PREFIX)/Tuple%.scala,$(LIBRARY_FILES))
TUPLE_TEMPLATE		 = $(TUPLE_PREFIX)/Tuple.scala.tmpl

# lamp library
LAMPLIB_ROOT		 = $(PROJECT_SOURCEDIR)/ch/epfl/lamp
LAMPLIB_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/lamplib.lst)
LAMPLIB_SOURCES		+= $(LAMPLIB_LIST:%=$(LAMPLIB_ROOT)/%)
LAMPLIB_JC_FILES	+= $(LAMPLIB_SOURCES)

# meta programming
META_ROOT		 = $(PROJECT_SOURCEDIR)/meta
META_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/meta.lst)
META_SOURCES		+= $(META_LIST:%=$(META_ROOT)/%)
META_JC_FILES		+= $(META_SOURCES)

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
COMPILER_JC_CLASSPATH	 = $(PROJECT_CLASSPATH):$(BCEL_JARFILE):$(MSIL_JARFILE):$(FJBG_JARFILE)

# scala interpreter
INTERPRETER_ROOT	 = $(PROJECT_SOURCEDIR)/scalai
INTERPRETER_LIST	 = $(call READLIST,$(PROJECT_LISTDIR)/interpreter.lst)
INTERPRETER_SOURCES	+= $(INTERPRETER_LIST:%=$(INTERPRETER_ROOT)/%)
INTERPRETER_JC_FILES	 = $(INTERPRETER_SOURCES)

# scaladoc
SCALADOC_ROOT		 = $(PROJECT_SOURCEDIR)/scaladoc
SCALADOC_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/scaladoc.lst)
SCALADOC_SOURCES	+= $(SCALADOC_LIST:%=$(SCALADOC_ROOT)/%)
SCALADOC_JC_FILES	 = $(SCALADOC_SOURCES)

# dtd2scala
DTD2SCALA_ROOT		 = $(PROJECT_SOURCEDIR)/dtd2scala
DTD2SCALA_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/dtd2scala.lst)
DTD2SCALA_SOURCES	+= $(DTD2SCALA_LIST:%=$(DTD2SCALA_ROOT)/%)
DTD2SCALA_JC_FILES	 = $(DTD2SCALA_SOURCES)

# scala library
LIBRARY_ROOT		 = $(RUNTIME_ROOT)
LIBRARY_LIST		 = $(call READLIST,$(PROJECT_LISTDIR)/library.lst)
LIBRARY_FILES		+= $(LIBRARY_LIST:%=$(LIBRARY_ROOT)/%)

# java compilation
JC_COMPILER		 = PICO
JC_OUTPUTDIR		 = $(PROJECT_OUTPUTDIR)
JC_CLASSPATH		 = $(PROJECT_CLASSPATH)

##############################################################################
# Commands


all		: $(PROJECT_OUTPUTDIR)
all		: scripts
all		: lamplib
all		: meta
all		: generate
all		: runtime
all		: compiler
all		: interpreter
all		: scaladoc
all		: dtd2scala
all		: library

force		: fastclean
	@$(make) all

fastclean	:
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(RM) .generated
	$(RM) .latest-dtd2scala
	$(RM) .latest-interpreter
	$(RM) .latest-compiler
	$(RM) .latest-runtime
	$(RM) .latest-scaladoc
	$(RM) .latest-generate
	$(RM) .latest-meta
	$(RM) .latest-lamplib

clean		: fastclean
	$(RM) -r $(PROJECT_OUTPUTDIR)/*

distclean	: clean
	$(RM) .latest-*
	$(RM) $(SCRIPTS_WRAPPER_LINKS)
	$(RM) $(SCRIPTS_WRAPPER)
	$(RM) -r $(PROJECT_OUTPUTDIR)
	$(RM) $(PROJECT_JAR_ARCHIVE)
	$(RM) $(ROOT)/support/latex/*.class

scripts		: $(SCRIPTS_WRAPPER_LINKS)
lamplib		: .latest-lamplib
meta		: .latest-meta
generate	: .latest-generate
runtime		: .latest-runtime
compiler	: .latest-compiler
interpreter	: .latest-interpreter
scaladoc	: .latest-scaladoc
dtd2scala	: .latest-dtd2scala
library		: .latest-library

.PHONY		: fastclean
.PHONY		: scripts
.PHONY		: lamplib
.PHONY		: meta
.PHONY		: generate
.PHONY		: runtime
.PHONY		: compiler
.PHONY		: interpreter
.PHONY		: scaladoc
.PHONY		: dtd2scala
.PHONY		: library

##############################################################################
# Targets

.latest-lamplib		: $(LAMPLIB_JC_FILES)
	@$(make) jc target=LAMPLIB LAMPLIB_JC_FILES='$?'
	touch $@

.latest-meta		: $(META_JC_FILES)
	@$(make) jc target=META META_JC_FILES='$?'
	$(RM) .latest-runtime
	$(RM) .latest-compiler
	touch $@

.latest-generate	: .latest-meta
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(strip $(JAVA) -cp $(JC_OUTPUTDIR) \
	    meta.GenerateAll $(PROJECT_SOURCEDIR) .generated)
	touch $@

.latest-runtime		: $(RUNTIME_JC_FILES)
	@$(make) jc target=RUNTIME RUNTIME_JC_FILES='$?'
	touch $@

.latest-compiler	: $(COMPILER_JC_FILES)
	@$(make) jc target=COMPILER COMPILER_JC_FILES='$?'
	touch $@

.latest-interpreter	: $(INTERPRETER_JC_FILES)
	@$(make) jc target=INTERPRETER INTERPRETER_JC_FILES='$?'
	touch $@

.latest-scaladoc	: $(SCALADOC_JC_FILES)
	@$(make) jc target=SCALADOC SCALADOC_JC_FILES='$?'
	touch $@

.latest-dtd2scala	: $(DTD2SCALA_JC_FILES)
	@$(make) jc target=DTD2SCALA DTD2SCALA_JC_FILES='$?'
	touch $@

.latest-library		: $(LIBRARY_FILES)
	touch $@

##############################################################################
# Rules

$(PROJECT_OUTPUTDIR)	:
	$(MKDIR) -p $(dir $(PROJECT_OUTPUTDIR))
	$(if $(PROJECT_OUTPUTDIR_LINK),\
	    $(LN) -s $(PROJECT_OUTPUTDIR_LINK) $(PROJECT_OUTPUTDIR),\
	    $(MKDIR) $(PROJECT_OUTPUTDIR))

$(SCRIPTS_WRAPPER)	: MACRO_VERSION           ?= "development version"
$(SCRIPTS_WRAPPER)	: MACRO_RUNTIME_SOURCES   ?= $(PROJECT_SOURCEDIR:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_RUNTIME_CLASSES   ?= $(PROJECT_OUTPUTDIR:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_DTD2SCALA_CLASSES ?= $(PROJECT_OUTPUTDIR:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_BCEL_CLASSES      ?= $(BCEL_JARFILE:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_FJBG_CLASSES      ?= $(FJBG_JARFILE:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_MSIL_CLASSES      ?= $(MSIL_JARFILE:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_XERCES_CLASSES    ?= $(XERCES_JARFILE:$(PROJECT_ROOT)%=$$PREFIX%)
$(SCRIPTS_WRAPPER)	: MACRO_JAVA_ARGS         ?= -enableassertions
$(SCRIPTS_WRAPPER)	: $(SCRIPTS_WRAPPER).tmpl $(PROJECT_ROOT)/Makefile
	$(RM) $@
	@$(ECHO) "Generating file $@"
	@$(SED) \
	    -es@{#VERSION#}@'$(MACRO_VERSION)'@g \
	    -es@{#RUNTIME_SOURCES#}@'$(MACRO_RUNTIME_SOURCES)'@g \
	    -es@{#RUNTIME_CLASSES#}@'$(MACRO_RUNTIME_CLASSES)'@g \
	    -es@{#DTD2SCALA_CLASSES#}@'$(MACRO_RUNTIME_CLASSES)'@g \
	    -es@{#BCEL_CLASSES#}@'$(MACRO_BCEL_CLASSES)'@g \
	    -es@{#FJBG_CLASSES#}@'$(MACRO_FJBG_CLASSES)'@g \
	    -es@{#MSIL_CLASSES#}@'$(MACRO_MSIL_CLASSES)'@g \
	    -es@{#XERCES_CLASSES#}@'$(MACRO_XERCES_CLASSES)'@g \
	    -es@{#JAVA_ARGS#}@'$(MACRO_JAVA_ARGS)'@g \
	    $@.tmpl > $@
	@macros=`$(SED) -n -es'@.*{#\(.*\)#}.*@\1@p' < $@`; \
	if [ -n "$$macros" ]; then \
	    $(ECHO) "error: there are undefined macros: $$macros"; \
	    $(RM) $@; \
	    exit 1; \
	fi;
	$(CHMOD) 755 $@

$(SCRIPTS_WRAPPER_LINKS): $(SCRIPTS_WRAPPER)
	@if [ ! -h $@ ]; then \
	    $(call RUN,$(LN) -s $(notdir $(SCRIPTS_WRAPPER)) $@); \
	fi

$(FUNCTION_FILES)	: .latest-meta $(FUNCTION_TEMPLATE)
	$(RM) .latest-generate
	@$(make) generate

$(TUPLE_FILES)		: .latest-meta $(TUPLE_TEMPLATE)
	$(RM) .latest-generate
	@$(make) generate

%			: .latest-meta %.tmpl
	$(RM) .latest-generate
	@$(make) generate

$(PROJECT_JAR_ARCHIVE)	: .latest-lamplib
$(PROJECT_JAR_ARCHIVE)	: .latest-runtime
$(PROJECT_JAR_ARCHIVE)	: .latest-compiler
$(PROJECT_JAR_ARCHIVE)	: .latest-interpreter
$(PROJECT_JAR_ARCHIVE)	: .latest-scaladoc
$(PROJECT_JAR_ARCHIVE)	: .latest-dtd2scala
$(PROJECT_JAR_ARCHIVE)	:
	@$(MAKE) jar target=PROJECT

##############################################################################
# Includes

include $(ROOT)/support/make/jc.mk
include $(ROOT)/support/make/jar.mk

##############################################################################
