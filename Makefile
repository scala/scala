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

# project java archive

PROJECT_JAR_ARCHIVE	 = $(ROOT)/lib/$(PROJECT_NAME).jar
PROJECT_JAR_MANIFEST	 = $(PROJECT_SOURCEDIR)/MANIFEST
PROJECT_JAR_INPUTDIR	 = $(PROJECT_OUTPUTDIR)
PROJECT_JAR_FILES	+= scala
PROJECT_JAR_FILES	+= scalac
PROJECT_JAR_FILES	+= scalai

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
FUNCTION_TEMPLATE	 = $(FUNCTION_PREFIX)/Function.java.tmpl

TUPLE_PREFIX		 = $(LIBRARY_ROOT)
TUPLE_FILES		+= $(filter $(TUPLE_PREFIX)/Tuple%.scala,$(LIBRARY_FILES))
TUPLE_TEMPLATE		 = $(TUPLE_PREFIX)/Tuple.scala.tmpl

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

# scala documents

DOCUMENTS_ROOT		 = $(PROJECT_ROOT)/doc
DOCUMENTS_FILES		+= $(DOCUMENTS_ROOT)/reference/reference.pdf
DOCUMENTS_FILES		+= $(DOCUMENTS_ROOT)/reference/examples.pdf

# scala examples

EXAMPLES_ROOT		 = $(PROJECT_SOURCEDIR)/examples
EXAMPLES_LIST		+= $(call READLIST, $(PROJECT_LISTDIR)/examples.lst)
EXAMPLES_FILES		 = $(EXAMPLES_LIST:%=$(EXAMPLES_ROOT)/%)

# emacs scala-mode

EMACS_ROOT		 = $(PROJECT_ROOT)/support/emacs
EMACS_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/emacs.lst)
EMACS_FILES		 = $(EMACS_LIST:%=$(EMACS_ROOT)/%)

# scala test

TEST_ROOT		 = $(PROJECT_ROOT)/test
TEST_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/test-pos.lst)
TEST_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/test-neg.lst)
TEST_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/test-run.lst)
TEST_FILES		 = $(TEST_LIST:%=$(TEST_ROOT)/%)

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
XARGS			?= xargs
JAVA			?= java

PICO			?= pico
PICO_FLAGS		+= -make -source 1.4

##############################################################################
# Functions

RUN			 = echo '$(1)'; $(1) || exit $$?
READLIST		 = $(shell cat $(1) | grep -v "^\#")

make			+= $(MAKE) MAKELEVEL=$(MAKELEVEL) --no-print-directory

##############################################################################
# Commands

all		: $(PROJECT_OUTPUTDIR)
all		: scripts
all		: meta
all		: generate
all		: runtime
all		: compiler
all		: interpreter
all		: library

force		: fastclean
	@$(make) all

fastclean	:
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(RM) .generated
	$(RM) .latest-interpreter
	$(RM) .latest-compiler
	$(RM) .latest-runtime
	$(RM) .latest-generate
	$(RM) .latest-meta

clean		: fastclean
	$(RM) -r $(PROJECT_OUTPUTDIR)/*

distclean	: clean
	$(RM) .latest-*
	$(RM) $(SCRIPTS_WRAPPER_LINKS)
	$(RM) -r $(PROJECT_OUTPUTDIR)
	$(RM) $(PROJECT_JAR_ARCHIVE)
	$(RM) $(ROOT)/support/latex/*.class

fixcvs		:
	$(strip \
	    $(FIND) . -type f -perm +a=x | \
	    $(GREP) -v '.*/bin/.*' | \
	    $(XARGS) -r $(CHMOD) a-x)

scripts		: $(SCRIPTS_WRAPPER_LINKS)
meta		: .latest-meta
generate	: .latest-generate
runtime		: .latest-runtime
compiler	: .latest-compiler
interpreter	: .latest-interpreter
library		: .latest-library

.PHONY		: all
.PHONY		: force
.PHONY		: fastclean
.PHONY		: clean
.PHONY		: distclean
.PHONY		: fixcvs
.PHONY		: scripts
.PHONY		: meta
.PHONY		: generate
.PHONY		: runtime
.PHONY		: compiler
.PHONY		: interpreter
.PHONY		: library

##############################################################################
# Targets

.latest-meta		: $(META_JC_FILES)
	@$(MAKE) jc target=META META_JC_FILES='$?'
	$(RM) .latest-runtime
	$(RM) .latest-compiler
	touch $@

.latest-generate	: .latest-meta
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(strip $(JAVA) -cp $(JC_OUTPUTDIR) \
	    meta.GenerateAll $(PROJECT_SOURCEDIR) .generated)
	touch $@

.latest-runtime		: $(RUNTIME_JC_FILES)
	@$(MAKE) jc target=RUNTIME RUNTIME_JC_FILES='$(filter %.java,$?)'
	touch $@

.latest-compiler	: $(COMPILER_JC_FILES)
	@$(make) jc target=COMPILER COMPILER_JC_FILES='$(filter %.java,$?)'
	touch $@

.latest-interpreter	: $(INTERPRETER_JC_FILES)
	@$(make) jc target=INTERPRETER INTERPRETER_JC_FILES='$?'
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

$(SCRIPTS_WRAPPER_LINKS):
	$(LN) -s $(SCRIPTS_WRAPPER_NAME) $@;

$(FUNCTION_FILES)	: .latest-meta $(FUNCTION_TEMPLATE)
	$(RM) .latest-generate
	@$(make) generate

$(TUPLE_FILES)		: .latest-meta $(TUPLE_TEMPLATE)
	$(RM) .latest-generate
	@$(make) generate

%			: .latest-meta %.tmpl
	$(RM) .latest-generate
	@$(make) generate

$(PROJECT_JAR_ARCHIVE)	: .latest-runtime
$(PROJECT_JAR_ARCHIVE)	: .latest-compiler
$(PROJECT_JAR_ARCHIVE)	: .latest-interpreter
$(PROJECT_JAR_ARCHIVE)	:
	@$(MAKE) jar target=PROJECT

##############################################################################

include $(ROOT)/support/make/jc.mk
include $(ROOT)/support/make/jar.mk
include $(ROOT)/support/make/exec.mk
include $(ROOT)/support/make/grep.mk
include $(ROOT)/support/make/wc.mk

##############################################################################

##############################################################################
# beta versions
##############################################################################

INSTALL			 = $(ROOT)/support/scripts/install
MIRROR			 = $(ROOT)/support/scripts/mirror

INSTALL_VERSION		 = install-test
INSTALL_PREFIX		 = $(ROOT)/distribs/scala-test
INSTALL_BINDIR		 = $(INSTALL_PREFIX)/bin
INSTALL_DOCDIR		 = $(INSTALL_PREFIX)/doc
INSTALL_LIBDIR		 = $(INSTALL_PREFIX)/lib
INSTALL_DATADIR		 = $(INSTALL_PREFIX)/share
INSTALL_DATADIR_ROOT	 = $(INSTALL_DATADIR)/$(PROJECT_NAME)
INSTALL_DATADIR_LIBRARY	 = $(INSTALL_DATADIR_ROOT)/library
INSTALL_DATADIR_EXAMPLES = $(INSTALL_DATADIR_ROOT)/examples
INSTALL_DATADIR_TEST	 = $(INSTALL_DATADIR_ROOT)/test
INSTALL_DATADIR_EMACS	 = $(INSTALL_DATADIR)/emacs/site-lisp

install-clean		:
	$(RM) -r $(INSTALL_PREFIX)

install			: $(PROJECT_JAR_ARCHIVE)
	$(MAKE) -C $(DOCUMENTS_ROOT)/reference all
	$(INSTALL) -m 755 -d $(INSTALL_PREFIX)
	$(INSTALL) -m 755 -d $(INSTALL_BINDIR)
	$(INSTALL) -m 755 $(ROOT)/bin/.scala_wrapper $(INSTALL_BINDIR)
	@$(MAKE) SCRIPTS_PREFIX=$(INSTALL_BINDIR) scripts
	$(INSTALL) -m 755 -d $(INSTALL_DOCDIR)
	$(INSTALL) -m 644 $(DOCUMENTS_FILES) $(INSTALL_DOCDIR)
	$(INSTALL) -m 755 -d $(INSTALL_LIBDIR)
	$(INSTALL) -m 644 $(PROJECT_JAR_ARCHIVE) $(INSTALL_LIBDIR)
	$(INSTALL) -m 644 $(BCEL_LICENSE) $(INSTALL_LIBDIR)/bcel.LICENSE
	$(INSTALL) -m 644 $(BCEL_JARFILE) $(INSTALL_LIBDIR)/bcel.jar
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_ROOT)
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_LIBRARY)
	$(strip $(MIRROR) -m 644 -C $(LIBRARY_ROOT) $(LIBRARY_LIST:%='%') \
	    $(INSTALL_DATADIR_LIBRARY)/$(PROJECT_NAME))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_EXAMPLES)
	$(strip $(MIRROR) -m 644 -C $(EXAMPLES_ROOT) $(EXAMPLES_LIST) \
	    $(INSTALL_DATADIR_EXAMPLES))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)/$(PROJECT_NAME)/test
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)/$(PROJECT_NAME)/test/bin
	$(INSTALL) -m 755 $(ROOT)/test/bin/scala-test $(INSTALL_DATADIR_TEST)/bin
	$(strip $(MIRROR) -m 644 -C $(TEST_ROOT) $(TEST_LIST) \
	    $(INSTALL_DATADIR_TEST))
	$(strip $(MIRROR) -m 644 -C $(TEST_ROOT) \
	    $(shell cd $(TEST_ROOT); ls $(TEST_LIST:%.scala=%.check) 2>/dev/null) \
	    $(INSTALL_DATADIR_TEST))
	$(strip $(MIRROR) -m 644 -C $(TEST_ROOT) \
	    $(shell cd $(TEST_ROOT); ls $(TEST_LIST:%.scala=%.flags) 2>/dev/null) \
	    $(INSTALL_DATADIR_TEST))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_EMACS)
	$(strip $(MIRROR) -m 644 -C $(EMACS_ROOT) $(EMACS_LIST) \
	    $(INSTALL_DATADIR_EMACS))
	$(INSTALL) -m 644 $(ROOT)/README $(INSTALL_PREFIX)
	$(INSTALL) -m 644 $(ROOT)/LICENSE $(INSTALL_PREFIX)
	$(ECHO) $(INSTALL_VERSION) > $(INSTALL_PREFIX)/VERSION
	$(CHMOD) 644 $(INSTALL_PREFIX)/VERSION


install-windows		:
	@if [ ! -d "$(INSTALL_PREFIX)" ]; then \
	    echo "Could not find UNIX install '$(INSTALL_PREFIX)'"; \
	    exit 1; \
	fi
	$(UNIX2DOS) $(INSTALL_PREFIX)/README
	$(UNIX2DOS) $(INSTALL_PREFIX)/LICENSE
	$(UNIX2DOS) $(INSTALL_PREFIX)/VERSION
	$(UNIX2DOS) $(INSTALL_LIBDIR)/bcel.LICENSE
	$(TOUCH) $(INSTALL_PREFIX)/VERSION-$(INSTALL_VERSION)
	@root=`cd "$(INSTALL_PREFIX)"; pwd`; \
	for file in "" $(SCRIPTS_WRAPPER_ALIASES); do \
	    if [ -z "$$file" ]; then continue; fi; \
	    echo -n "Generating $$file.bat ... "; \
	    srcfile="$(ROOT)/support/windows/scala_wrapper"; \
	    nixfile="$(INSTALL_PREFIX)/bin/$$file"; \
	    winfile="$(INSTALL_PREFIX)/bin/$$file.bat"; \
	    nixexec=`SCALA_WRAPPER_EXEC=echo $$nixfile`; \
	    winexec="$$nixexec"; \
	    winexec=`echo "$$winexec" | sed -es"#$$root#%SCALA_HOME%#g"`; \
	    winexec=`echo "$$winexec" | tr '/' '\\\\' | tr ':' ';'`; \
	    $(RM) -f "$$winfile"; \
	    ( \
	        $(CAT) "$$srcfile-header.bat"; \
	        $(ECHO) "set VERSION=$(INSTALL_VERSION)"; \
	        $(ECHO) "set COMMAND=$$winexec"; \
	        $(CAT) "$$srcfile-footer.bat"; \
	    ) | $(UNIX2DOS) >> "$$winfile"; \
	    $(RM) "$$nixfile"; \
	    echo "done"; \
	done
	$(RM) $(INSTALL_PREFIX)/bin/.scala_wrapper
	$(FIND) $(INSTALL_DATADIR_LIBRARY) -type f -exec unix2dos "{}" ";"
	$(FIND) $(INSTALL_DATADIR_EXAMPLES) -type f -exec unix2dos "{}" ";"
	$(FIND) $(INSTALL_DATADIR_TEST) -type f -exec unix2dos "{}" ";"
	$(FIND) $(INSTALL_DATADIR_EMACS) -type f -exec unix2dos "{}" ";"

.PHONY	: install
.PHONY	: install-windows

##############################################################################

DISTRIB_REPOSITORY	 = $(ROOT)/distribs
DISTRIB_VERSION		:= $(shell $(DATE) -u "+%Y%m%d-%H%M%S")
DISTRIB_NAME		 = $(PROJECT_NAME)-$(DISTRIB_VERSION)
DISTRIB_PREFIX		 = $(DISTRIB_REPOSITORY)/$(DISTRIB_NAME)
DISTRIB_ARCHIVE		 = $(DISTRIB_REPOSITORY)/$(DISTRIB_NAME).tar.gz

distrib-extract		:
	@if [ ! -f "$(DISTRIB_ARCHIVE)" ]; then \
	    echo "Could not find source archive '$(DISTRIB_ARCHIVE)'"; \
	    exit 1; \
	fi
	@if [ -d "$(DISTRIB_PREFIX)" ]; then \
	    $(call RUN,$(RM) -rf $(DISTRIB_PREFIX)); \
	fi
	tar xvzf $(DISTRIB_ARCHIVE) -C $(DISTRIB_REPOSITORY)

distrib-build-unix	:
	@$(MAKE) INSTALL_PREFIX=$(DISTRIB_PREFIX) \
	    INSTALL_VERSION=$(DISTRIB_VERSION) install
	tar czf $(DISTRIB_ARCHIVE) -C $(DISTRIB_REPOSITORY) $(DISTRIB_NAME)

distrib-build-windows	:
	@$(MAKE) INSTALL_PREFIX=$(DISTRIB_PREFIX) \
	    INSTALL_VERSION=$(DISTRIB_VERSION) install-windows
	$(RM) -f $(DISTRIB_PREFIX).zip
	cd $(DISTRIB_REPOSITORY); \
	    $(ZIP) -q -r $(DISTRIB_NAME).zip $(DISTRIB_NAME)

distrib-clean		:
	$(RM) -rf $(DISTRIB_PREFIX)

distrib-unix		: distrib-build-unix
distrib-unix		: distrib-clean

distrib-windows		: DISTRIB_NAME=$(notdir $(ARGS:%.tar.gz=%))
distrib-windows 	: DISTRIB_VERSION=$(DISTRIB_NAME:scala-%=%)
distrib-windows 	: distrib-extract
distrib-windows 	: distrib-build-windows
distrib-windows 	: distrib-clean

distrib-all-oses	: distrib-build-unix
distrib-all-oses	: distrib-build-windows
distrib-all-oses	: distrib-clean

distrib-14		:
	@if [ -f "$(ROOT)/bin/.scala_wrapper.bak" ]; then \
	    echo "There is a backup of .scala_wrapper, maybe you should run: "; \
	    echo "make distrib-13-undo"; \
	    exit 1; \
	fi
	@$(MAKE) distrib-all-oses DISTRIB_VERSION=$(DISTRIB_VERSION);

distrib-13-undo		:
	@$(MAKE) clean;
	mv $(ROOT)/bin/.scala_wrapper.bak $(ROOT)/bin/.scala_wrapper

distrib-13		: PATH:=/home/linuxsoft/apps/java-1.3/bin:$(PATH)
distrib-13		:
	@if [ -f "$(ROOT)/bin/.scala_wrapper.bak" ]; then \
	    echo "Cannot save .scala_wrapper, there is already a backup," \
	      "maybe you should run: "; \
	    echo "make distrib-13-undo"; \
	    exit 1; \
	fi
	mv $(ROOT)/bin/.scala_wrapper $(ROOT)/bin/.scala_wrapper.bak
	$(SED) -e "s/-enableassertions//" $(ROOT)/bin/.scala_wrapper.bak \
	    > $(ROOT)/bin/.scala_wrapper
	@$(MAKE) clean;
	@$(MAKE) COMPILER_JC_FLAGS="$(COMPILER_JC_FLAGS) -target 1.3"
	@$(MAKE) distrib-all-oses DISTRIB_VERSION=$(DISTRIB_VERSION)-jdk-1.3;
	@$(MAKE) distrib-13-undo;

distrib			: distrib-14
#distrib			: distrib-13

.PHONY	: distrib-extract
.PHONY	: distrib-build-unix
.PHONY	: distrib-build-windows
.PHONY	: distrib-clean
.PHONY	: distrib-unix
.PHONY	: distrib-windows
.PHONY	: distrib-all-oses
.PHONY	: distrib-14
.PHONY	: distrib-13-undo
.PHONY	: distrib-13
.PHONY	: distrib

##############################################################################
