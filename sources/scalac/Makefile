# Makefile for the Scala compiler. Needs GNU Make.
# $Id$

# Main targets:
#   all (default)
#     Recompile every file that was modified after the latest
#     compilation ended. The date of the latest compilation end is
#     remembered through the files .latest-(compiler|runtime).
#   util
#     Recompile only utility files, used by the compiler.
#   compiler
#     Recompile only files which belong to the compiler.
#   runtime
#     Recompile only files which belong to the run-time.
#   regtest
#     Perform regression testing.
#   force
#     Recompile everything, no matter when source files were modified.
#   clean
#     Remove everything in the $(PROJECT_GENDIR)/scala directory.

# There are also targets to run grep on all files and to count the
# number of lines.

# Directory in which .class files should be put.
# BEWARE! The whole contents of the "scala" subdirectory in this
# directory is wiped out by a "make clean" so don't put anything else
# than automatically-generated .class files there.


##############################################################################
# Variables
##############################################################################

# makefile

ROOT			 = .
MAKE_HOME		 = $(ROOT)/make

# project

PROJECT_NAME		 = scala
PROJECT_MAKEDIR		 = $(PROJECT_ROOT)
PROJECT_SOURCES		+= $(RUNTIME_SOURCES)
PROJECT_SOURCES		+= $(COMPILER_SOURCES)
PROJECT_SOURCES		+= $(INTERPRETER_SOURCES)
PROJECT_ROOT		 = $(ROOT)
PROJECT_GENDIR		 = $(PROJECT_ROOT)/classes
PROJECT_CLASSPATH	 = $(PROJECT_GENDIR)

# BCEL (see http://jakarta.apache.org/bcel/)

BCEL_HOME		 = /home/linuxsoft/apps/BCEL-5
BCEL_JARFILE		 = $(BCEL_HOME)/bcel.jar
BCEL_LICENSE		 = $(BCEL_HOME)/LICENSE

# needed for the .NET backend

MSIL_HOME		 = /home/linuxsoft/apps/java2net
MSIL_JARFILE		 = $(MSIL_HOME)/msil.jar

# grep

GREP_ARGS		?= $(ARGS)

# regression tests

REGTEST			 = ./test/bin/scala-test
REGTEST_ARGS		?= $(ARGS)

# scripts

SCRIPTS_PREFIX		 = bin

SCRIPTS_LINKED_NAMES	+= socos
SCRIPTS_LINKED_NAMES	+= socos-debug
SCRIPTS_LINKED_NAMES	+= surus
SCRIPTS_LINKED_NAMES	+= surus-debug
SCRIPTS_LINKED_NAMES	+= siris
SCRIPTS_LINKED_NAMES	+= siris-debug

SCRIPTS_LINKED_FILES	 = $(SCRIPTS_LINKED_NAMES:%=$(SCRIPTS_PREFIX)/%)

# automatic generation of Function<n>.java and Tuple<n>.scala files

TEMPLATE_EXPANDER	 = ./bin/expand-template
TEMPLATE_DIR		 = ./sources/templates

FUNCTION_FILES		+= $(filter ./sources/scala/Function%.java,$(PROJECT_SOURCES))
FUNCTION_TEMPLATE	 = $(TEMPLATE_DIR)/Function.tmpl
FUNCTION_RULES		 = $(TEMPLATE_DIR)/Function.scm

TUPLE_FILES		+= $(filter $(LIBRARY_DIR)/scala/Tuple%.scala,$(LIBRARY_FILES))
TUPLE_TEMPLATE		 = $(TEMPLATE_DIR)/Tuple.tmpl
TUPLE_RULES		 = $(TEMPLATE_DIR)/Tuple.scm

# scala runtime

RUNTIME_SOURCES		+= $(call READLIST,Sources-runtime)
RUNTIME_JC_COMPILER	+= PICO

# scala compiler

COMPILER_SOURCES	+= $(call READLIST,Sources-compiler)
COMPILER_JC_COMPILER	 = PICO
COMPILER_JC_CLASSPATH    = $(PROJECT_CLASSPATH):$(BCEL_JARFILE):$(MSIL_JARFILE)

# scala interpreter

INTERPRETER_SOURCES	+= $(call READLIST,Sources-interpreter)
INTERPRETER_JC_COMPILER	 = PICO

# scala resources

RESOURCE_MIRROR_FLAGS	 = -m 644
RESOURCE_MIRROR_DSTDIR	 = $(PROJECT_GENDIR)
RESOURCE_MIRROR_SRCDIR	 = sources
RESOURCE_MIRROR_FILES	+= scala/compiler/jaco/resources/Messages.properties
RESOURCE_MIRROR_FILES	+= scala/compiler/jaco/resources/Settings.properties

# scala library

LIBRARY_DIR		 = $(PROJECT_ROOT)/scala-sources
LIBRARY_files		+= $(call READLIST, $(MAKE_HOME)/list/library.lst)
LIBRARY_FILES		 = $(LIBRARY_files:%=$(LIBRARY_DIR)/%)

# scala examples

EXAMPLES_DIR		 = $(PROJECT_ROOT)/examples
EXAMPLES_files		+= $(call READLIST, $(MAKE_HOME)/list/examples.lst)
EXAMPLES_FILES		 = $(EXAMPLES_files:%=$(EXAMPLES_DIR)/%)

# scala prelude

PRELUDE_DIR		 = $(LIBRARY_DIR)/prelude
PRELUDE_FILES		+= $(call READLIST,$(MAKE_HOME)/list/prelude.lst)
prelude_NAMES		+= $(notdir $(PRELUDE_FILES))
prelude_LINKS		+= $(prelude_NAMES:%=$(PRELUDE_DIR)/%)

# scala test

TEST_DIR		 = $(PROJECT_ROOT)/test
TEST_files		+= $(call READLIST, $(MAKE_HOME)/list/test-pos.lst)
TEST_files		+= $(call READLIST, $(MAKE_HOME)/list/test-neg.lst)
TEST_files		+= $(call READLIST, $(MAKE_HOME)/list/test-int.lst)
TEST_FILES		 = $(TEST_files:%=$(TEST_DIR)/%)

# emacs scala-mode

EMACS_DIR		 = $(PROJECT_ROOT)/support/emacs
EMACS_files		+= $(call READLIST, $(MAKE_HOME)/list/emacs.lst)
EMACS_FILES		 = $(EMACS_files:%=$(EMACS_DIR)/%)

# jar

PROJECT_JAR_ARCHIVE	 = lib/$(PROJECT_NAME).jar
PROJECT_JAR_MANIFEST	 = sources/MANIFEST
PROJECT_JAR_INPUTDIR	 = $(PROJECT_GENDIR)
PROJECT_JAR_FILES	+= scala

# java compilation

JC_OUTPUTDIR		 = $(PROJECT_GENDIR)
JC_CLASSPATH		 = $(PROJECT_CLASSPATH)

# Tools to use for the other targets.

ECHO			 = echo
CAT			 = cat
JAR			 = jar
GREP			 = grep
FIND			 = find
WC			 = wc
SED			 = sed
M4			 = m4
RM			 = rm
CP			 = cp
LN			 = ln
MKDIR			 = mkdir
TOUCH			 = touch
CHMOD			 = chmod
DIRNAME			 = dirname
BASENAME		 = basename
DATE			 = date
NICE			 = nice
ZIP			 = zip
UNIX2DOS		 = unix2dos

JAVAC			 = javac
JAVAC_FLAGS		+=

PICO			 = pico
PICO_FLAGS		+= -make -source 1.4

# private

RUN			 = echo $(1); $(1) || exit $$?
LOOKUP			 = $(if $($(source)_$(1)),$($(source)_$(1)),$($(1)))
MAKE			+= --no-print-directory

ifeq ($(shell which bash),/usr/local/bin/bash)
BASH			 = /bin/bash
else
BASH			 = bash
endif

READLIST		 = $(shell cat $(1) | grep -v "^\#")
DO_SCRIPT		 = $(MAKE_HOME)/scripts.bash
DO			 = @exec $(BASH) -c "source $(DO_SCRIPT)" $(1)
INSTALL			 = $(call DO,install)

##############################################################################
# Commands
##############################################################################

all		: scripts
all		: functions
all		: runtime
all		: compiler
all		: interpreter
all		: resources
all		: library

scripts		: $(SCRIPTS_LINKED_FILES)
functions	: $(FUNCTION_FILES)
runtime		: .latest-runtime
compiler	: .latest-compiler
interpreter	: .latest-interpreter
resources	: .latest-resources
library		: .latest-library
jar		: $(PROJECT_JAR_ARCHIVE)

clean		:
	$(RM) -f .latest-resources
	$(RM) -f .latest-runtime .latest-compiler .latest-interpreter
	$(RM) -rf $(PROJECT_GENDIR)/scala

distclean	:
	@$(MAKE) clean
	$(RM) -f .latest-library .latest-prelude
	$(RM) -f $(FUNCTION_FILES)
	$(RM) -f $(TUPLE_FILES)
	$(RM) -f $(prelude_LINKS)
	$(RM) -f $(PROJECT_JAR_ARCHIVE)
	$(RM) -f $(SCRIPTS_LINKED_FILES)

force		:
	$(RM) -f .latest-resources
	$(RM) -f .latest-runtime .latest-compiler .latest-interpreter
	@$(MAKE) all

wc		:
	@$(WC) $(PROJECT_SOURCES)

grep		:
	@$(if $(GREP_ARGS), \
             $(GREP) $(GREP_ARGS) $(PROJECT_SOURCES), \
             echo "You should put the arguments for grep in the ARGS variable")

regtest		:
	$(NICE) -10 $(REGTEST) $(REGTEST_ARGS)

show-duplicates	:
	@rm -f check.tmp.log check.mkf.log
	@for filename in $(PROJECT_SOURCES); do \
	  echo $$filename | tr " " "\n" >> check.tmp.log; \
	done
	@sort check.tmp.log > check.mkf.log
	@uniq -d check.mkf.log

show-missing	:
	@rm -f check.tmp.log check.mkf.log check.lst.log
	@for filename in $(PROJECT_SOURCES); do \
	  echo $$filename | tr " " "\n" >> check.tmp.log; \
	done
	@sort check.tmp.log > check.mkf.log
	@$(FIND) . -name "*.java" | sort > check.lst.log
	@comm -1 -3 check.mkf.log check.lst.log 
	@rm -f check.tmp.log check.mkf.log check.lst.log

.PHONY		: all clean distclean force
.PHONY		: scripts runtime compiler interpreter resources jar
.PHONY		: wc grep regtest
.PHONY		: show-duplicates show-missing

##############################################################################
# Targets
##############################################################################

.latest-runtime		: $(RUNTIME_SOURCES)
	@$(MAKE) .do-jc source=RUNTIME JC_FILES='$?'
	touch $@

.latest-compiler	: $(COMPILER_SOURCES) 
	@$(MAKE) .do-jc source=COMPILER JC_FILES='$?'
	touch $@

.latest-interpreter	: $(INTERPRETER_SOURCES)
	@$(MAKE) .do-jc source=INTERPRETER JC_FILES='$?'
	touch $@

.latest-resources	: $(RESOURCE_MIRROR_SOURCES)
	@$(MAKE) .do-mirror source=RESOURCE
	touch $@

.latest-library		: $(LIBRARY_FILES)
	touch $@

##############################################################################
# Rules
##############################################################################

.build-scripts: $(SCRIPTS_LINKED_FILES)

$(SCRIPTS_LINKED_FILES):
	$(LN) -s .scala_wrapper $@;

$(FUNCTION_FILES): $(FUNCTION_TEMPLATE) $(FUNCTION_RULES)
	$(TEMPLATE_EXPANDER) $(FUNCTION_RULES) $(FUNCTION_TEMPLATE) $@

$(TUPLE_FILES): $(TUPLE_TEMPLATE) $(TUPLE_RULES)
	$(TEMPLATE_EXPANDER) $(TUPLE_RULES) $(TUPLE_TEMPLATE) $@

$(PROJECT_JAR_ARCHIVE)	: .latest-runtime
$(PROJECT_JAR_ARCHIVE)	: .latest-compiler
$(PROJECT_JAR_ARCHIVE)	: .latest-interpreter
$(PROJECT_JAR_ARCHIVE)	: .latest-resources
	@$(MAKE) .do-jar source=PROJECT

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
	@if [ -n "$(JC_OUTPUTDIR)" -a ! -d "$(JC_OUTPUTDIR)" ]; then \
	    $(call RUN,$(INSTALL) -m 755 -d "$(JC_OUTPUTDIR)"); \
	fi
	$(strip $(JC_compiler) $(JC_compiler_flags) $(JC_FLAGS) \
	    $(JC_OUTPUTDIR:%=-d %) $(JC_CLASSPATH:%=-classpath %) \
	    $(JC_FILES))

##############################################################################
# JAR - build java archive
##############################################################################
#
# JAR			 = archive command
# JAR_FLAGS		+= archive flags
# JAR_ARCHIVE		 = archive file
# JAR_MANIFEST		 = manifest file
# JAR_INPUTDIR		 = directory containing the files to archive
# JAR_FILES		+= files to store (relative to JAR_INPUTDIR)
#
##############################################################################

# setup default values

JAR			?= jar
JAR_FLAGS		?= v

# lookup actual values

JAR			:= $(call LOOKUP,JAR)
JAR_FLAGS		:= $(call LOOKUP,JAR_FLAGS)
JAR_ARCHIVE		:= $(call LOOKUP,JAR_ARCHIVE)
JAR_MANIFEST		:= $(call LOOKUP,JAR_MANIFEST)
JAR_INPUTDIR		:= $(call LOOKUP,JAR_INPUTDIR)
JAR_FILES		:= $(call LOOKUP,JAR_FILES)

# rules

.do-jar:
	$(strip $(JAR) c$(JAR_FLAGS)f$(JAR_MANIFEST:%=m) $(JAR_ARCHIVE) \
	    $(JAR_MANIFEST) $(JAR_INPUTDIR:%=-C %) $(JAR_FILES))

##############################################################################
# MIRROR - mirror files
##############################################################################
#
# MIRROR		 = mirror command
# MIRROR_FLAGS		+= mirror flags
# MIRROR_DSTDIR		 = directory in which the files are to be mirrored
# MIRROR_SRCDIR		 = directory containing the files to mirror
# MIRROR_FILES		+= files to mirror
#
##############################################################################

# setup default values

MIRROR			?= $(call DO,mirror)

# lookup actual values

MIRROR			:= $(call LOOKUP,MIRROR)
MIRROR_FLAGS		:= $(call LOOKUP,MIRROR_FLAGS)
MIRROR_DSTDIR		:= $(call LOOKUP,MIRROR_DSTDIR)
MIRROR_SRCDIR		:= $(call LOOKUP,MIRROR_SRCDIR)
MIRROR_FILES		:= $(call LOOKUP,MIRROR_FILES)

# rules

.do-mirror:
	$(strip $(MIRROR) $(MIRROR_FLAGS) $(MIRROR_SRCDIR:%=-C %) \
	    $(MIRROR_FILES) $(MIRROR_DSTDIR))

##############################################################################
##############################################################################

##############################################################################
# beta versions
##############################################################################

INSTALL_VERSION		 = install-test
INSTALL_PREFIX		 = $(ROOT)/distribs/scala-test
INSTALL_BINDIR		 = $(INSTALL_PREFIX)/bin
INSTALL_LIBDIR		 = $(INSTALL_PREFIX)/lib
INSTALL_DATADIR		 = $(INSTALL_PREFIX)/share
INSTALL_DATADIR_ROOT	 = $(INSTALL_DATADIR)/$(PROJECT_NAME)
INSTALL_DATADIR_LIBRARY	 = $(INSTALL_DATADIR_ROOT)/library
INSTALL_DATADIR_EXAMPLES = $(INSTALL_DATADIR_ROOT)/examples
INSTALL_DATADIR_TEST	 = $(INSTALL_DATADIR_ROOT)/test
INSTALL_DATADIR_EMACS	 = $(INSTALL_DATADIR)/emacs/site-lisp

install-unix		: $(PROJECT_JAR_ARCHIVE)
	$(INSTALL) -m 755 -d $(INSTALL_PREFIX)
	$(INSTALL) -m 755 -d $(INSTALL_BINDIR)
	$(INSTALL) -m 755 ./bin/.scala_wrapper $(INSTALL_BINDIR)
	@$(MAKE) SCRIPTS_PREFIX=$(INSTALL_BINDIR) .build-scripts
	$(INSTALL) -m 755 -d $(INSTALL_LIBDIR)
	$(INSTALL) -m 644 $(PROJECT_JAR_ARCHIVE) $(INSTALL_LIBDIR)
	$(INSTALL) -m 644 $(BCEL_LICENSE) $(INSTALL_LIBDIR)/bcel.LICENSE
	$(INSTALL) -m 644 $(BCEL_JARFILE) $(INSTALL_LIBDIR)/bcel.jar
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_ROOT)
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_LIBRARY)
	$(strip $(MIRROR) -m 644 -C $(LIBRARY_DIR) $(LIBRARY_files) \
	    $(INSTALL_DATADIR_LIBRARY))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_EXAMPLES)
	$(strip $(MIRROR) -m 644 -C $(EXAMPLES_DIR) $(EXAMPLES_files) \
	    $(INSTALL_DATADIR_EXAMPLES))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)/$(PROJECT_NAME)/test
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR)/$(PROJECT_NAME)/test/bin
	$(INSTALL) -m 755 ./test/bin/scala-test $(INSTALL_DATADIR_TEST)/bin
	$(strip $(MIRROR) -m 644 -C $(TEST_DIR) $(TEST_files) \
	    $(INSTALL_DATADIR_TEST))
	$(strip $(MIRROR) -m 644 -C $(TEST_DIR) \
	    $(shell cd $(TEST_DIR); ls $(TEST_files:%.scala=%.check) 2>/dev/null) \
	    $(INSTALL_DATADIR_TEST))
	$(strip $(MIRROR) -m 644 -C $(TEST_DIR) \
	    $(shell cd $(TEST_DIR); ls $(TEST_files:%.scala=%.flags) 2>/dev/null) \
	    $(INSTALL_DATADIR_TEST))
	$(INSTALL) -m 755 -d $(INSTALL_DATADIR_EMACS)
	$(strip $(MIRROR) -m 644 -C $(EMACS_DIR) $(EMACS_files) \
	    $(INSTALL_DATADIR_EMACS))
	$(INSTALL) -m 644 ./README $(INSTALL_PREFIX)
	$(ECHO) $(INSTALL_VERSION) > $(INSTALL_PREFIX)/VERSION
	$(CHMOD) 644 $(INSTALL_PREFIX)/VERSION

install-windows		:
	@if [ ! -d "$(INSTALL_PREFIX)" ]; then \
	    echo "Could not find UNIX install '$(INSTALL_PREFIX)'"; \
	    exit 1; \
	fi
	$(UNIX2DOS) $(INSTALL_PREFIX)/README
	$(UNIX2DOS) $(INSTALL_PREFIX)/VERSION
	$(UNIX2DOS) $(INSTALL_LIBDIR)/bcel.LICENSE
	$(TOUCH) $(INSTALL_PREFIX)/VERSION-$(INSTALL_VERSION)
	@root=`cd "$(INSTALL_PREFIX)"; pwd`; \
	for file in "" $(SCRIPTS_LINKED_NAMES); do \
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

##############################################################################

DISTRIB_REPOSITORY	 = $(ROOT)/distribs
DISTRIB_VERSION		:= $(shell $(DATE) --utc "+%Y%m%d-%H%M%S")
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
	    INSTALL_VERSION=$(DISTRIB_VERSION) install-unix
	tar cvzf $(DISTRIB_ARCHIVE) -C $(DISTRIB_REPOSITORY) $(DISTRIB_NAME)

distrib-build-windows	:
	@$(MAKE) INSTALL_PREFIX=$(DISTRIB_PREFIX) \
	    INSTALL_VERSION=$(DISTRIB_VERSION) install-windows
	$(RM) -f $(DISTRIB_PREFIX).zip
	cd $(DISTRIB_REPOSITORY); $(ZIP) -r $(DISTRIB_NAME).zip $(DISTRIB_NAME)

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

#distrib-13		: PATH:=/home/linuxsoft/apps/java-1.3/bin:$(PATH)
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
distrib			: distrib-13

.PHONY	: install-unix install-windows
.PHONY	: distrib-extract distrib-build-unix distrib-build-windows
.PHONY	: distrib-unix distrib-windows distrib

##############################################################################
