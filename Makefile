############################################################-*-Makefile-*-####
# Makefile for the Scala Compiler
##############################################################################
# $Id$

##############################################################################
# Configuration

ROOT			 = .

include $(ROOT)/Makefile.import

##############################################################################
# Variables - build contexts

# command and target prefix
prefix			?= main
latest			 = $(LATEST_PREFIX)-$(prefix)-

meta_PREFIX		 = meta
meta_OBJECTDIR		 = $(PROJECT_OBJECTDIR)/$(meta_PREFIX)
meta_JC_OUTPUTDIR	 = $(meta_OBJECTDIR)/classes
meta_JC_CLASSPATH	 = $(meta_JC_OUTPUTDIR)
meta_SC_BOOTCLASSPATH	 =
meta_SCALAC		 = $(SCALAC)

boot_PREFIX		 = boot
boot_OBJECTDIR		 = $(PROJECT_OBJECTDIR)/$(boot_PREFIX)
boot_LIBRARY_CLASSDIR	 = $(main_LIBRARY_CLASSDIR)
boot_TOOLS_CLASSDIR	 = $(boot_OBJECTDIR)/lib/$(TOOLS_NAME)
boot_JC_OUTPUTDIR	 = $(boot_TOOLS_CLASSDIR)
boot_JC_CLASSPATH	 = $(boot_JC_OUTPUTDIR):$(boot_LIBRARY_CLASSDIR)
boot_SC_BOOTCLASSPATH	 =
boot_SCALAC		 = $(SCALAC)
boot_SCALADOC		 = $(SCALADOC)
boot_SCALA_CMD		 = $(SCALA)

main_PREFIX		 = main
main_OBJECTDIR		 = $(PROJECT_OBJECTDIR)/$(main_PREFIX)
main_LIBRARY_CLASSDIR	 = $(main_OBJECTDIR)/lib/$(LIBRARY_NAME)
main_TOOLS_CLASSDIR	 = $(main_OBJECTDIR)/lib/$(TOOLS_NAME)
main_JC_OUTPUTDIR	 = $(main_TOOLS_CLASSDIR)
main_JC_CLASSPATH	 = $(main_JC_OUTPUTDIR):$(main_LIBRARY_CLASSDIR)
main_SC_BOOTCLASSPATH	 = $(JRE_JARFILE)
main_SCALAC		 = $(boot_OBJECTDIR)/bin/scalac
main_SCALADOC		 = $(main_OBJECTDIR)/bin/scaladoc
main_SCALA_CMD		 = $(main_OBJECTDIR)/bin/scala

test_PREFIX		 = test
test_OBJECTDIR		 = $(PROJECT_OBJECTDIR)/$(test_PREFIX)
test_LIBRARY_CLASSDIR	 = $(test_OBJECTDIR)/lib/$(LIBRARY_NAME)
test_TOOLS_CLASSDIR	 = $(test_OBJECTDIR)/lib/$(TOOLS_NAME)
test_JC_OUTPUTDIR	 = $(test_TOOLS_CLASSDIR)
test_JC_CLASSPATH	 = $(test_JC_OUTPUTDIR):$(test_LIBRARY_CLASSDIR)
test_SC_BOOTCLASSPATH	 = $(JRE_JARFILE)
test_SCALAC		 = $(main_OBJECTDIR)/bin/scalac
test_SCALADOC		 = $(test_OBJECTDIR)/bin/scaladoc
test_SCALA_CMD		 = $(test_OBJECTDIR)/bin/scala

##############################################################################
# Variables

# java compilation defaults
JC_COMPILER		 = PICO
JC_OUTPUTDIR		 = $($(prefix)_JC_OUTPUTDIR)
JC_CLASSPATH		 = $($(prefix)_JC_CLASSPATH)

# scala compilation defaults
SC_COMPILER		 = $(prefix)_SCALAC
SC_OUTPUTDIR		 = $(JC_OUTPUTDIR)
SC_CLASSPATH		 = $(JC_CLASSPATH):$(PROJECT_SOURCEDIR)
SC_BOOTCLASSPATH	 = $($(prefix)_SC_BOOTCLASSPATH)

# scala documentation compilation defaults
SDC_COMPILER		 = $(prefix)_SCALADOC

##############################################################################
# Commands - global

all			: $(LATEST_PREFIX)-boot-all
all			: main

force			:
	@$(make) all

fastclean		:
	$(RM) $(LATEST_PREFIX)-main*

clean			:
	$(RM) -r $(main_OBJECTDIR)

distclean		:
	$(RM) $(LATEST_PREFIX)-*
	$(RM) -r $(PROJECT_OBJECTDIR)

##############################################################################
# Commands - project building

meta			: meta.all
boot			: boot.all
main			: main.all
test			: test.all

scripts			: main.scripts
lamplib			: main.lamplib
library			: main.library
library-msil		: main.library-msil
library-doc		: main.library-doc
util			: main.util
scalac			: main.scalac
scalai			: main.scalai
scaladoc		: main.scaladoc
scalap			: main.scalap
dtd2scala		: main.dtd2scala
scala4ant		: main.scala4ant
scalatest		: main.scalatest
servlet			: main.servlet
tools			: main.tools

$(prefix).all		: $(latest)all
$(prefix).scripts	: $(latest)scripts
$(prefix).lamplib	: $(latest)lamplib
$(prefix).meta		: $(latest)meta
$(prefix).library	: $(latest)library
$(prefix).library-msil	: $(latest)library-msil
$(prefix).library-doc	: $(latest)library-sdc
$(prefix).util		: $(latest)util
$(prefix).scalac	: $(latest)scalac
$(prefix).scalai	: $(latest)scalai
$(prefix).scaladoc	: $(latest)scaladoc
$(prefix).scalap	: $(latest)scalap
$(prefix).dtd2scala	: $(latest)dtd2scala
$(prefix).scala4ant	: $(latest)scala4ant
$(prefix).scalatest	: $(latest)scalatest
$(prefix).servlet	: $(latest)servlet
$(prefix).tools		: $(latest)tools

meta.%			: ; @$(make) prefix="meta" $@;
boot.% 			: ; @$(make) prefix="boot" $@;
main.%			: ; @$(make) prefix="main" $@;
test.%			: ; @$(make) prefix="test" $@;

.PHONY			: meta
.PHONY			: boot
.PHONY			: main
.PHONY			: test

.PHONY			: scripts
.PHONY			: lamplib
.PHONY			: library
.PHONY			: library-msil
.PHONY			: library-doc
.PHONY			: util
.PHONY			: scalac
.PHONY			: scalai
.PHONY			: scaladoc
.PHONY			: scalap
.PHONY			: dtd2scala
.PHONY			: scala4ant
.PHONY			: scalatest
.PHONY			: servlet
.PHONY			: tools

.PHONY			: $(prefix).all
.PHONY			: $(prefix).scripts
.PHONY			: $(prefix).lamplib
.PHONY			: $(prefix).meta
.PHONY			: $(prefix).library
.PHONY			: $(prefix).library-msil
.PHONY			: $(prefix).library-doc
.PHONY			: $(prefix).util
.PHONY			: $(prefix).scalac
.PHONY			: $(prefix).scalai
.PHONY			: $(prefix).scaladoc
.PHONY			: $(prefix).scalap
.PHONY			: $(prefix).dtd2scala
.PHONY			: $(prefix).scala4ant
.PHONY			: $(prefix).scalatest
.PHONY			: $(prefix).servlet
.PHONY			: $(prefix).tools

##############################################################################
# Commands - version management

version-set		:
	@if [ -z "$(VERSION)" ]; then \
	    echo "Usage: $(MAKE) version-set VERSION=<version>"; \
	    exit 1; \
	else \
	    $(call RUN,$(VERSION_SCRIPT) $(VERSION_FILE) set $(VERSION)); \
	    $(make) scripts; \
	fi

version-update		:
	$(VERSION_SCRIPT) $(VERSION_FILE) update
	@$(make) scripts

version-increment	:
	$(VERSION_SCRIPT) $(VERSION_FILE) increment
	@$(make) scripts

.PHONY			: version-set
.PHONY			: version-update
.PHONY			: version-increment

##############################################################################
# Commands - CVS management

cvs-fix-perms		:
	$(strip \
	    $(FIND) . -type f -perm +a=x | \
	    $(GREP) -v '.*/bin/.*' | \
	    $(XARGS) -r $(CHMOD) a-x)

.PHONY			: cvs-fix-perms

##############################################################################
# Commands - source file generation

generate		: meta.generate

$(prefix).generate	: $(LATEST_PREFIX)-meta-all
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(strip $(JAVA) -cp $(call CYGWIN_PATH,$(JC_CLASSPATH)) \
	    meta.GenerateAll $(call CYGWIN_FILE,$(PROJECT_SOURCEDIR)) \
	    .generated)

.PHONY			: generate
.PHONY			: $(prefix).generate

##############################################################################
# Targets - top-level prefixes

ifeq ($(prefix),meta)
$(latest)all		: $(latest)lamplib
$(latest)all		: $(latest)meta
endif

ifeq ($(prefix),boot)
$(latest)all		: $(latest)scripts
$(latest)all		: $(latest)lamplib
$(latest)all		: $(latest)util
$(latest)all		: $(latest)scalac
endif

ifeq ($(prefix),main)
$(latest)all		: $(latest)scripts
$(latest)all		: $(latest)lamplib
$(latest)all		: $(latest)library
$(latest)all		: $(latest)tools
endif

ifeq ($(prefix),test)
$(latest)all		: $(latest)lamplib
$(latest)all		: $(latest)library
$(latest)all		: $(latest)tools
endif

$(latest)all		:
	$(TOUCH) $@

$(LATEST_PREFIX)-meta-%	: ; @$(make) prefix="meta" $@
$(LATEST_PREFIX)-boot-%	: ; @$(make) prefix="boot" $@
$(LATEST_PREFIX)-main-%	: ; @$(make) prefix="main" $@
$(LATEST_PREFIX)-test-%	: ; @$(make) prefix="test" $@

##############################################################################
# Targets - scala scripts

SCRIPTS_PREFIX		 = $($(prefix)_OBJECTDIR)
SCRIPTS_BINARYDIR	 = $(SCRIPTS_PREFIX)/bin
SCRIPTS_TEMPLATE_NAME	 = $(SCRIPTS_WRAPPER_NAME).tmpl
SCRIPTS_TEMPLATE_FILE	 = $(PROJECT_BINARYDIR)/$(SCRIPTS_TEMPLATE_NAME)
SCRIPTS_WRAPPER_NAME	 = .scala_wrapper
SCRIPTS_WRAPPER_FILE	 = $(SCRIPTS_BINARYDIR)/$(SCRIPTS_WRAPPER_NAME)
SCRIPTS_ALIASES_NAMES	+= scala
SCRIPTS_ALIASES_NAMES	+= scala-debug
SCRIPTS_ALIASES_NAMES	+= scala-info
SCRIPTS_ALIASES_NAMES	+= scalac
SCRIPTS_ALIASES_NAMES	+= scalac-debug
SCRIPTS_ALIASES_NAMES	+= scaladoc
SCRIPTS_ALIASES_NAMES	+= scaladoc-debug
SCRIPTS_ALIASES_NAMES	+= scalarun
SCRIPTS_ALIASES_NAMES	+= scalarun-debug
SCRIPTS_ALIASES_NAMES	+= scalaint
SCRIPTS_ALIASES_NAMES	+= scalaint-debug
SCRIPTS_ALIASES_NAMES	+= dtd2scala
SCRIPTS_ALIASES_NAMES	+= scalap
SCRIPTS_ALIASES_NAMES	+= scalatest
SCRIPTS_ALIASES_FILES	+= $(SCRIPTS_ALIASES_NAMES:%=$(SCRIPTS_BINARYDIR)/%)
SCRIPTS_MACRO		 = -es@{\#$(1)\#}@'"$(MACRO_$(1):$(SCRIPTS_PREFIX)/%=$$PREFIX/%)"'@g

$(latest)scripts	: $(SCRIPTS_ALIASES_FILES)
	$(TOUCH) $@

$(SCRIPTS_ALIASES_FILES): $(SCRIPTS_WRAPPER_FILE)
	@if [ ! -h $@ ]; then \
	    $(call RUN,$(LN) -s $(notdir $(SCRIPTS_WRAPPER_FILE)) $@); \
	fi

$(SCRIPTS_WRAPPER_FILE)	: ROOT                    := $(shell cd $(ROOT) && pwd)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_VERSION           ?= $(PROJECT_VERSION)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_LIBRARY_SOURCES   ?= $(PROJECT_SOURCEDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_LIBRARY_CLASSES   ?= $(LIBRARY_CLASSDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_TOOLS_CLASSES     ?= $(TOOLS_CLASSDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_FJBG_CLASSES      ?= $(FJBG_JARFILE)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_MSIL_CLASSES      ?= $(MSIL_JARFILE)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_JAVA_CMD          ?= java
$(SCRIPTS_WRAPPER_FILE)	: MACRO_JAVA_ARGS         ?= -enableassertions
$(SCRIPTS_WRAPPER_FILE)	: MACRO_SCALA_CMD         ?= $($(prefix)_SCALA_CMD)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_SCALA_ARGS        ?= -enableassertions
$(SCRIPTS_WRAPPER_FILE)	: MACRO_TEST_SRCDIR       ?= $(PROJECT_TESTDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_TEST_OBJDIR       ?= ""
$(SCRIPTS_WRAPPER_FILE)	: MACRO_TEST_ASSEMBLIES   ?= $(MSIL_HOME)
$(SCRIPTS_WRAPPER_FILE)	: $(VERSION_FILE)
$(SCRIPTS_WRAPPER_FILE)	: $(PROJECT_ROOT)/Makefile
$(SCRIPTS_WRAPPER_FILE)	: $(PROJECT_ROOT)/Makefile.config
$(SCRIPTS_WRAPPER_FILE)	: $(PROJECT_ROOT)/Makefile.import
$(SCRIPTS_WRAPPER_FILE)	: $(PROJECT_ROOT)/Makefile.private
$(SCRIPTS_WRAPPER_FILE)	: $(SCRIPTS_TEMPLATE_FILE)
	@[ -d $(@D) ] || $(call RUN,$(MKDIR) -p $(@D))
	@[ -e $@ ] || $(call RUN,$(RM) $@)
	@$(ECHO) "Generating file $@"
	@$(SED) \
	    $(call SCRIPTS_MACRO,VERSION) \
	    $(call SCRIPTS_MACRO,LIBRARY_SOURCES) \
	    $(call SCRIPTS_MACRO,LIBRARY_CLASSES) \
	    $(call SCRIPTS_MACRO,TOOLS_CLASSES) \
	    $(call SCRIPTS_MACRO,FJBG_CLASSES) \
	    $(call SCRIPTS_MACRO,MSIL_CLASSES) \
	    $(call SCRIPTS_MACRO,JAVA_CMD) \
	    $(call SCRIPTS_MACRO,JAVA_ARGS) \
	    $(call SCRIPTS_MACRO,SCALA_CMD) \
	    $(call SCRIPTS_MACRO,SCALA_ARGS) \
	    $(call SCRIPTS_MACRO,TEST_SRCDIR) \
	    $(call SCRIPTS_MACRO,TEST_OBJDIR) \
	    $(call SCRIPTS_MACRO,TEST_ASSEMBLIES) \
	    $(SCRIPTS_TEMPLATE_FILE) > $@
	@macros=`$(SED) -n -es'@.*{#\(.*\)#}.*@\1@p' < $@`; \
	if [ -n "$$macros" ]; then \
	    $(ECHO) "error: there are undefined macros: $$macros"; \
	    $(RM) $@; \
	    exit 1; \
	fi;
	$(CHMOD) 555 $@

##############################################################################
# Targets - lamp library

PROJECT_SOURCES		+= $(LAMPLIB_SOURCES)
LAMPLIB_ROOT		 = $(PROJECT_SOURCEDIR)/ch/epfl/lamp
LAMPLIB_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/lamplib.lst)
LAMPLIB_SOURCES		+= $(LAMPLIB_LIST:%=$(LAMPLIB_ROOT)/%)
LAMPLIB_JC_FILES	+= $(LAMPLIB_SOURCES)

$(latest)lamplib	: $(latest)lamplib-jc
$(latest)lamplib	:
	$(TOUCH) $@

$(latest)lamplib-jc	: $(LAMPLIB_JC_FILES)
	@$(make) jc target=LAMPLIB LAMPLIB_JC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - meta library

PROJECT_SOURCES		+= $(META_SOURCES)
META_ROOT		 = $(PROJECT_SOURCEDIR)/meta
META_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/meta.lst)
META_SOURCES		+= $(META_LIST:%=$(META_ROOT)/%)
META_JC_FILES		+= $(META_SOURCES)

$(latest)meta		: $(latest)meta-jc
$(latest)meta		:
	$(TOUCH) $@

$(latest)meta-jc	: $(META_JC_FILES)
	@$(make) jc target=META META_JC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala library

PROJECT_SOURCES		+= $(LIBRARY_SOURCES)
LIBRARY_NAME		 = $(PROJECT_NAME)
LIBRARY_ROOT		 = $(PROJECT_SOURCEDIR)/scala
LIBRARY_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/library.lst)
LIBRARY_SOURCES		+= $(LIBRARY_LIST:%=$(LIBRARY_ROOT)/%)
LIBRARY_CLASSDIR	 = $($(prefix)_LIBRARY_CLASSDIR)
LIBRARY_JC_FILES	+= $(filter %.java,$(LIBRARY_SOURCES))
LIBRARY_JC_FLAGS	+= $(JC_FLAGS) -scala-hack
LIBRARY_JC_OUTPUTDIR	 = $(LIBRARY_CLASSDIR)
LIBRARY_JC_CLASSPATH	 = $(LIBRARY_JC_OUTPUTDIR)
LIBRARY_SC_FILES	+= $(filter %.scala,$(LIBRARY_SOURCES))
LIBRARY_SC_OUTPUTDIR	 = $(LIBRARY_JC_OUTPUTDIR)
LIBRARY_SC_CLASSPATH	 = $(LIBRARY_JC_CLASSPATH):$(PROJECT_SOURCEDIR)
LIBRARY_SDC_FLAGS	+= -windowtitle "Scala Library Documentation"
LIBRARY_SDC_FLAGS	+= -doctitle "Scala<br/>$(PROJECT_VERSION)"
LIBRARY_SDC_FILES	+= $(LIBRARY_SC_FILES)
LIBRARY_SDC_OUTPUTDIR	 = $($(prefix)_OBJECTDIR)/doc/api
LIBRARY_JAR_ARCHIVE	 = $(LIBRARY_CLASSDIR).jar
LIBRARY_JAR_INPUTDIR	 = $(LIBRARY_CLASSDIR)
LIBRARY_JAR_FILES	+= .

$(latest)library	: $(latest)library-jc
$(latest)library	: $(latest)library-sc
$(latest)library	:
	$(TOUCH) $@

$(latest)library-jc	: $(LIBRARY_JC_FILES)
	@$(make) jc target=LIBRARY LIBRARY_JC_FILES='$(subst $$,$$$$,$?)'
	$(TOUCH) $@

$(latest)library-sc	: $(LIBRARY_SC_FILES)
	@$(make) sc target=LIBRARY LIBRARY_SC_FILES='$(subst $$,$$$$,$?)'
	$(TOUCH) $@

$(latest)library-sdc	: $(LIBRARY_SDC_FILES)
	@$(make) sdc target=LIBRARY
	$(TOUCH) $@

ifneq ($(LIBRARY_JAR_ARCHIVE),.jar)

$(LIBRARY_JAR_ARCHIVE)	: $(latest)library
$(LIBRARY_JAR_ARCHIVE)	:
	@$(make) jar target=LIBRARY

endif

##############################################################################
# Targets - scala library

LIBRARY_MSIL_ROOT	 = $(LIBRARY_ROOT)
LIBRARY_MSIL_LIST	+= $(call READLIST,$(PROJECT_LISTDIR)/library-msil.lst)
LIBRARY_MSIL_SOURCES	+= $(LIBRARY_MSIL_LIST:%=$(LIBRARY_MSIL_ROOT)/%)
LIBRARY_MSIL_SC_FILES	+= $(filter %.scala,$(LIBRARY_MSIL_SOURCES))
LIBRARY_MSIL_SC_FLAGS	+= -r $(MSCORLIB_DLLFILE):$(VJSCOR_DLLFILE):$(VJSLIB_DLLFILE)
LIBRARY_MSIL_SC_FLAGS	+= -uniqid -o scalalibx -g
LIBRARY_MSIL_SC_TARGET	 = msil

$(latest)library-msil	: $(latest)library-msil-sc
$(latest)library-msil	:
	$(TOUCH) $@

$(latest)library-msil-sc: $(LIBRARY_MSIL_SC_FILES)
	@$(make) sc target=LIBRARY_MSIL \
	    LIBRARY_MSIL_SC_FILES='$(subst $$,$$$$,$?)'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - util

PROJECT_SOURCES		+= $(UTIL_SOURCES)
UTIL_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/util
UTIL_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/util.lst)
UTIL_SOURCES		+= $(UTIL_LIST:%=$(UTIL_ROOT)/%)
UTIL_JC_FILES		+= $(filter %.java,$(UTIL_SOURCES))
UTIL_SC_FILES		+= $(filter %.scala,$(UTIL_SOURCES))

$(latest)util		: $(latest)util-jc
$(latest)util		: $(latest)util-sc
$(latest)util		:
	$(TOUCH) $@

$(latest)util-jc	: $(UTIL_JC_FILES)
	@$(make) jc target=UTIL UTIL_JC_FILES='$?'
	$(TOUCH) $@

$(latest)util-sc	: $(UTIL_SC_FILES)
	@$(make) sc target=UTIL UTIL_SC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - compiler

PROJECT_SOURCES		+= $(SCALAC_SOURCES)
SCALAC_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalac
SCALAC_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalac.lst)
SCALAC_SOURCES		+= $(SCALAC_LIST:%=$(SCALAC_ROOT)/%)
SCALAC_JC_FILES		+= $(filter %.java,$(SCALAC_SOURCES))
SCALAC_JC_CLASSPATH	 = $(JC_CLASSPATH):$(MSIL_JARFILE):$(FJBG_JARFILE)
SCALAC_SC_FILES		+= $(filter %.scala,$(SCALAC_SOURCES))
SCALAC_SC_CLASSPATH	 = $(SCALAC_JC_CLASSPATH):$(PROJECT_SOURCEDIR)

$(latest)scalac		: $(latest)scalac-jc
$(latest)scalac		: $(latest)scalac-sc
$(latest)scalac		:
	$(TOUCH) $@

$(latest)scalac-jc	: $(SCALAC_JC_FILES)
	@$(make) jc target=SCALAC SCALAC_JC_FILES='$?'
	$(TOUCH) $@

$(latest)scalac-sc	: $(SCALAC_SC_FILES)
	@$(make) sc target=SCALAC SCALAC_SC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - interpreter

PROJECT_SOURCES		+= $(SCALAI_SOURCES)
SCALAI_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalai
SCALAI_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalai.lst)
SCALAI_SOURCES		+= $(SCALAI_LIST:%=$(SCALAI_ROOT)/%)
SCALAI_JC_FILES		 = $(SCALAI_SOURCES)

$(latest)scalai		: $(latest)scalai-jc
$(latest)scalai		:
	$(TOUCH) $@

$(latest)scalai-jc	: $(SCALAI_JC_FILES)
	@$(make) jc target=SCALAI SCALAI_JC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - scaladoc

PROJECT_SOURCES		+= $(SCALADOC_SOURCES)
SCALADOC_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scaladoc
SCALADOC_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scaladoc.lst)
SCALADOC_SOURCES	+= $(SCALADOC_LIST:%=$(SCALADOC_ROOT)/%)
SCALADOC_JC_FILES	+= $(filter %.java,$(SCALADOC_SOURCES))
SCALADOC_SC_FILES	+= $(filter %.scala,$(SCALADOC_SOURCES))
SCALADOC_RSRC_LIST	+= resources/script.js
SCALADOC_RSRC_LIST	+= resources/style.css
SCALADOC_RSRC_LIST	+= resources/xhtml-lat1.ent
SCALADOC_RSRC_LIST	+= resources/xhtml-special.ent
SCALADOC_RSRC_LIST	+= resources/xhtml-symbol.ent
SCALADOC_RSRC_LIST	+= resources/xhtml1-transitional.dtd
SCALADOC_RSRC_FILES	+= $(SCALADOC_RSRC_LIST:%=$(SCALADOC_ROOT)/%)
SCALADOC_RSRC_OUTPUTDIR	 = $(SCALADOC_ROOT:$(PROJECT_SOURCEDIR)/%=$(JC_OUTPUTDIR)/%)

$(latest)scaladoc	: $(latest)scaladoc-jc
$(latest)scaladoc	: $(latest)scaladoc-sc
$(latest)scaladoc	: $(latest)scaladoc-rsrc
$(latest)scaladoc	:
	$(TOUCH) $@

$(latest)scaladoc-jc	: $(SCALADOC_JC_FILES)
	@$(make) jc target=SCALADOC SCALADOC_JC_FILES='$?'
	$(TOUCH) $@

$(latest)scaladoc-sc	: $(SCALADOC_SC_FILES)
	@$(make) sc target=SCALADOC SCALADOC_SC_FILES='$?'
	$(TOUCH) $@

$(latest)scaladoc-rsrc	: $(SCALADOC_RSRC_FILES)
	$(strip $(MIRROR) -m 644 -C $(SCALADOC_ROOT) $(SCALADOC_RSRC_LIST) \
	    $(SCALADOC_RSRC_OUTPUTDIR))
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - scalap

PROJECT_SOURCES		+= $(SCALAP_SOURCES)
SCALAP_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalap
SCALAP_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalap.lst)
SCALAP_SOURCES		+= $(SCALAP_LIST:%=$(SCALAP_ROOT)/%)
SCALAP_SC_FILES	 	+= $(SCALAP_SOURCES)

$(latest)scalap		: $(latest)scalap-sc
$(latest)scalap		:
	$(TOUCH) $@

$(latest)scalap-sc	: $(SCALAP_SC_FILES)
	@$(make) sc target=SCALAP SCALAP_SC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - dtd2scala

PROJECT_SOURCES		+= $(DTD2SCALA_SOURCES)
DTD2SCALA_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/dtd2scala
DTD2SCALA_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/dtd2scala.lst)
DTD2SCALA_SOURCES	+= $(DTD2SCALA_LIST:%=$(DTD2SCALA_ROOT)/%)
DTD2SCALA_SC_FILES	+= $(filter %.scala,$(DTD2SCALA_SOURCES))
DTD2SCALA_RSRC_LIST	+= $(filter %.xml,$(DTD2SCALA_LIST))
DTD2SCALA_RSRC_FILES	+= $(filter %.xml,$(DTD2SCALA_SOURCES))
DTD2SCALA_RSRC_OUTPUTDIR = $(DTD2SCALA_ROOT:$(PROJECT_SOURCEDIR)/%=$(JC_OUTPUTDIR)/%)

$(latest)dtd2scala	: $(latest)dtd2scala-sc
$(latest)dtd2scala	: $(latest)dtd2scala-rsrc
$(latest)dtd2scala	:
	$(TOUCH) $@

$(latest)dtd2scala-sc	: $(DTD2SCALA_SC_FILES)
	@$(make) sc target=DTD2SCALA DTD2SCALA_SC_FILES='$?'
	$(TOUCH) $@

$(latest)dtd2scala-rsrc	: $(DTD2SCALA_RSRC_FILES)
	$(strip $(MIRROR) -m 644 -C $(DTD2SCALA_ROOT) $(DTD2SCALA_RSRC_LIST) \
	    $(DTD2SCALA_RSRC_OUTPUTDIR))
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - scala4ant

PROJECT_SOURCES		+= $(SCALA4ANT_SOURCES)
SCALA4ANT_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scala4ant
SCALA4ANT_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scala4ant.lst)
SCALA4ANT_SOURCES	+= $(SCALA4ANT_LIST:%=$(SCALA4ANT_ROOT)/%)
SCALA4ANT_SC_FILES	+= $(SCALA4ANT_SOURCES)
SCALA4ANT_SC_CLASSPATH	 = $(SC_CLASSPATH):$(ANT_JARFILE)

$(latest)scala4ant	: $(latest)scala4ant-sc
$(latest)scala4ant	:
	$(TOUCH) $@

$(latest)scala4ant-sc	: $(SCALA4ANT_SC_FILES)
	@$(make) sc target=SCALA4ANT SCALA4ANT_SC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - scalatest

PROJECT_SOURCES		+= $(SCALATEST_SOURCES)
SCALATEST_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalatest
SCALATEST_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalatest.lst)
SCALATEST_SOURCES	+= $(SCALATEST_LIST:%=$(SCALATEST_ROOT)/%)
SCALATEST_JC_FILES	+= $(SCALATEST_SOURCES)

$(latest)scalatest	: $(latest)scalatest-jc
$(latest)scalatest	:
	$(TOUCH) $@

$(latest)scalatest-jc	: $(SCALATEST_JC_FILES)
	@$(make) jc target=SCALATEST SCALATEST_JC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools - servlet engine

PROJECT_SOURCES		+= $(SERVLET_SOURCES)
SERVLET_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/servlet
SERVLET_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/servlet.lst)
SERVLET_SOURCES		+= $(SERVLET_LIST:%=$(SERVLET_ROOT)/%)
SERVLET_SC_FILES	+= $(SERVLET_SOURCES)

$(latest)servlet	: $(latest)servlet-sc
$(latest)servlet	:
	$(TOUCH) $@

$(latest)servlet-sc	: $(SERVLET_SC_FILES)
	@$(make) sc target=SERVLET SERVLET_SC_FILES='$?'
	$(TOUCH) $@

##############################################################################
# Targets - scala tools

TOOLS_NAME		 = tools
TOOLS_CLASSDIR		 = $($(prefix)_TOOLS_CLASSDIR)
TOOLS_JAR_ARCHIVE	 = $(TOOLS_CLASSDIR).jar
TOOLS_JAR_INPUTDIR	 = $(TOOLS_CLASSDIR)
TOOLS_JAR_FILES		+= .

$(latest)tools		: $(latest)lamplib
$(latest)tools		: $(latest)util
$(latest)tools		: $(latest)scalac
$(latest)tools		: $(latest)scalai
$(latest)tools		: $(latest)scaladoc
$(latest)tools		: $(latest)scalap
$(latest)tools		: $(latest)dtd2scala
$(latest)tools		: $(latest)scala4ant
$(latest)tools		: $(latest)scalatest
$(latest)tools		: $(latest)servlet
$(latest)tools		:
	$(TOUCH) $@

ifneq ($(TOOLS_JAR_ARCHIVE),.jar)

$(TOOLS_JAR_ARCHIVE)	: $(latest)tools
$(TOOLS_JAR_ARCHIVE)	:
	@$(make) jar target=TOOLS

endif

##############################################################################
# Targets - template expansion

# generation of Function<n>.scala
FUNCTION_FILES		+= $(filter $(LIBRARY_ROOT)/Function%.scala,$(LIBRARY_SOURCES))
FUNCTION_TEMPLATE	 = $(LIBRARY_ROOT)/Function.scala.tmpl

# generation of Tuple<n>.scala
TUPLE_FILES		+= $(filter $(LIBRARY_ROOT)/Tuple%.scala,$(LIBRARY_SOURCES))
TUPLE_TEMPLATE		 = $(LIBRARY_ROOT)/Tuple.scala.tmpl

distclean		: distclean.generate
distclean.generate	:
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(RM) .generated

$(FUNCTION_FILES)	: $(META_SOURCES) $(FUNCTION_TEMPLATE)
	@$(make) generate

$(TUPLE_FILES)		: $(META_SOURCES) $(TUPLE_TEMPLATE)
	@$(make) generate

%			: $(META_SOURCES) %.tmpl
	@$(make) generate

##############################################################################
# Includes

include $(PROJECT_ROOT)/Makefile.distrib
include $(PROJECT_SUPPORTDIR)/make/jc.mk
include $(PROJECT_SUPPORTDIR)/make/jar.mk
include $(PROJECT_SUPPORTDIR)/make/sc.mk
include $(PROJECT_SUPPORTDIR)/make/sdc.mk

##############################################################################
# Beta code

show-missing-library	:
	@$(RM) /tmp/check.tmp.log /tmp/check.mkf.log /tmp/check.lst.log
	@for filename in $(LIBRARY_SOURCES:%='%'); do \
	  echo $$filename | $(TR) " " "\n" >> /tmp/check.tmp.log; \
	done
	@$(SORT) /tmp/check.tmp.log > /tmp/check.mkf.log
	@$(FIND) $(LIBRARY_ROOT) -name "tools" -prune -o \( -name '*.java' -o -name '*.scala' \) -print | $(SORT) > /tmp/check.lst.log
	@$(COMM) -1 -3 /tmp/check.mkf.log /tmp/check.lst.log
	@$(RM) /tmp/check.tmp.log /tmp/check.mkf.log /tmp/check.lst.log


show-missing-test	:
	@$(RM) /tmp/check.tmp.log /tmp/check.mkf.log /tmp/check.lst.log
	@for filename in $(TEST_FILES:%='%'); do \
	  echo $$filename | $(TR) " " "\n" >> /tmp/check.tmp.log; \
	done
	@$(SORT) /tmp/check.tmp.log > /tmp/check.mkf.log
	@$(FIND) $(TEST_ROOT) -name '*.scala' | $(SORT) > /tmp/check.lst.log
	@$(COMM) -1 -3 /tmp/check.mkf.log /tmp/check.lst.log
	@$(RM) /tmp/check.tmp.log /tmp/check.mkf.log /tmp/check.lst.log

.PHONY			: show-missing

##############################################################################
