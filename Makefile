############################################################-*-Makefile-*-####
# Makefile for the Scala Compiler
##############################################################################
# $Id$

##############################################################################
# Configuration

ROOT			 = .

include $(ROOT)/Makefile.import

##############################################################################
# Variables

# command and target prefix
LATEST			 = $(PROJECT_LATEST_PREFIX)
prefix			?=
latest			 = $(LATEST)$(prefix)

# java compilation defaults
JC_COMPILER		 = PICO
JC_OUTPUTDIR		 = $(PROJECT_OUTPUTDIR)
JC_CLASSPATH		 = $(PROJECT_CLASSPATH)

# scala compilation defaults
SC_COMPILER		 = SCALAC
SC_OUTPUTDIR		 = $(JC_OUTPUTDIR)
SC_CLASSPATH		 = $(JC_CLASSPATH)

##############################################################################
# Commands - global

all		: sources
all		: bootstrap
all		: system
all		: scalai
all		: scaladoc
all		: scalap
all		: dtd2scala
all		: scala4ant
all		: scalatest
all		: servlet

force		:
	@$(make) all

fastclean	:
	$(RM) $(LATEST)*

clean		:
	$(RM) -r $(PROJECT_APIDOCDIR)
	$(RM) -r $(PROJECT_BOOTTESTDIR)
	$(RM) -r $(PROJECT_OUTPUTDIR)
	$(RM) -r $(PROJECT_BOOTSTRAPDIR)

##############################################################################
# Commands - project building

bootstrap		: $(LATEST)bootstrap
boottest		: $(LATEST)boottest

$(prefix)sources	: $(prefix)lamplib
$(prefix)sources	: $(prefix)meta

$(prefix)system		: $(prefix)scripts
$(prefix)system		: $(prefix)lamplib
$(prefix)system		: $(prefix)library
$(prefix)system		: $(prefix)util
$(prefix)system		: $(prefix)scalac

$(prefix)scripts	: $(latest)scripts
$(prefix)lamplib	: $(latest)lamplib-jc
$(prefix)meta		: $(latest)meta-jc
$(prefix)library	: $(latest)library-jc
$(prefix)library	: $(latest)library-sc
$(prefix)library-msil	: $(latest)library-msil-sc
$(prefix)library-doc	: $(latest)library-sdc
$(prefix)util		: $(latest)util-jc
$(prefix)util		: $(latest)util-sc
$(prefix)scalac		: $(latest)scalac-jc
$(prefix)scalac		: $(latest)scalac-sc
$(prefix)scalai		: $(latest)scalai-jc
$(prefix)scaladoc	: $(latest)scaladoc-jc
$(prefix)scaladoc	: $(latest)scaladoc-sc
$(prefix)scaladoc	: $(latest)scaladoc-rsrc
$(prefix)scalap		: $(latest)scalap-sc
$(prefix)dtd2scala	: $(latest)dtd2scala-sc
$(prefix)dtd2scala	: $(latest)dtd2scala-rsrc
$(prefix)scala4ant	: $(latest)scala4ant-sc
$(prefix)scalatest	: $(latest)scalatest-jc
$(prefix)servlet	: $(latest)servlet-sc

.PHONY			: bootstrap
.PHONY			: boottest
.PHONY			: $(prefix)sources
.PHONY			: $(prefix)system
.PHONY			: $(prefix)scripts
.PHONY			: $(prefix)lamplib
.PHONY			: $(prefix)meta
.PHONY			: $(prefix)library
.PHONY			: $(prefix)library-msil
.PHONY			: $(prefix)library-doc
.PHONY			: $(prefix)util
.PHONY			: $(prefix)scalac
.PHONY			: $(prefix)scalai
.PHONY			: $(prefix)scaladoc
.PHONY			: $(prefix)scalap
.PHONY			: $(prefix)dtd2scala
.PHONY			: $(prefix)scala4ant
.PHONY			: $(prefix)scalatest
.PHONY			: $(prefix)servlet

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
# Targets - bootstraping

$(LATEST)bootstrap	:
	@$(make) bootstrap-system;
	touch $@

$(LATEST)boottest	:
	@$(make) boottest-system;
	touch $@

ifeq ($(prefix),)

bootstrap-%		\
$(LATEST)bootstrap-%	:
	@$(make) \
	    INSTALL_PREFIX=$(PROJECT_BOOTSTRAPDIR) \
	    SCRIPTS_PREFIX=$(PROJECT_BOOTSTRAPDIR)/bin \
	    JC_OUTPUTDIR=$(PROJECT_BOOTSTRAPDIR)/classes \
	    JC_CLASSPATH=$(PROJECT_BOOTSTRAPDIR)/classes \
	    LIBRARY_SCALAC=$(BOOTSTRAP_SCALAC) \
	    prefix="bootstrap-" $@;

boottest-%		:
$(LATEST)boottest-%	:
	@$(make) \
	    INSTALL_PREFIX=$(PROJECT_BOOTTESTDIR) \
	    SCRIPTS_PREFIX=$(PROJECT_BOOTTESTDIR)/bin \
	    JC_OUTPUTDIR=$(PROJECT_BOOTTESTDIR)/classes \
	    JC_CLASSPATH=$(PROJECT_BOOTTESTDIR)/classes \
	    LIBRARY_SCALAC=$(PROJECT_BINARYDIR)/scalac \
	    prefix="boottest-" $@;
	touch $@

endif

##############################################################################
# Targets - scala scripts

SCRIPTS_PREFIX		 = $(PROJECT_BINARYDIR)
SCRIPTS_TEMPLATE_NAME	 = $(SCRIPTS_WRAPPER_NAME).tmpl
SCRIPTS_TEMPLATE_FILE	 = $(PROJECT_BINARYDIR)/$(SCRIPTS_TEMPLATE_NAME)
SCRIPTS_WRAPPER_NAME	 = .scala_wrapper
SCRIPTS_WRAPPER_FILE	 = $(SCRIPTS_PREFIX)/$(SCRIPTS_WRAPPER_NAME)
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
SCRIPTS_ALIASES_FILES	+= $(SCRIPTS_ALIASES_NAMES:%=$(SCRIPTS_PREFIX)/%)
SCRIPTS_MACRO		 = -es@{\#$(1)\#}@'"$(MACRO_$(1):$(INSTALL_PREFIX)/%=$$PREFIX/%)"'@g

distclean		: distclean.scripts
distclean.scripts	:
	$(RM) $(SCRIPTS_ALIASES_FILES)
	$(RM) $(SCRIPTS_WRAPPER_FILE)

$(latest)scripts	: $(SCRIPTS_ALIASES_FILES)
	touch $@

$(SCRIPTS_ALIASES_FILES): $(SCRIPTS_WRAPPER_FILE)
	@if [ ! -h $@ ]; then \
	    $(call RUN,$(LN) -s $(notdir $(SCRIPTS_WRAPPER_FILE)) $@); \
	fi

$(SCRIPTS_WRAPPER_FILE)	: INSTALL_PREFIX          ?= $(PROJECT_ROOT)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_VERSION           ?= $(PROJECT_VERSION)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_RUNTIME_SOURCES   ?= $(PROJECT_SOURCEDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_RUNTIME_CLASSES   ?= $(JC_OUTPUTDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_TOOLS_CLASSES     ?= $(JC_OUTPUTDIR)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_FJBG_CLASSES      ?= $(FJBG_JARFILE)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_MSIL_CLASSES      ?= $(MSIL_JARFILE)
$(SCRIPTS_WRAPPER_FILE)	: MACRO_JAVA_ARGS         ?= -enableassertions
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
	    $(call SCRIPTS_MACRO,RUNTIME_SOURCES) \
	    $(call SCRIPTS_MACRO,RUNTIME_CLASSES) \
	    $(call SCRIPTS_MACRO,TOOLS_CLASSES) \
	    $(call SCRIPTS_MACRO,FJBG_CLASSES) \
	    $(call SCRIPTS_MACRO,MSIL_CLASSES) \
	    $(call SCRIPTS_MACRO,JAVA_ARGS) \
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

$(latest)lamplib-jc	: $(LAMPLIB_JC_FILES)
	@$(make) jc target=LAMPLIB LAMPLIB_JC_FILES='$?'
	touch $@

##############################################################################
# Targets - meta library

PROJECT_SOURCES		+= $(META_SOURCES)
META_ROOT		 = $(PROJECT_SOURCEDIR)/meta
META_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/meta.lst)
META_SOURCES		+= $(META_LIST:%=$(META_ROOT)/%)
META_JC_FILES		+= $(META_SOURCES)

$(latest)meta-jc	: $(META_JC_FILES)
	@$(make) jc target=META META_JC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala library

PROJECT_SOURCES		+= $(LIBRARY_SOURCES)
LIBRARY_ROOT		 = $(PROJECT_SOURCEDIR)/scala
LIBRARY_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/library.lst)
LIBRARY_MSIL_LIST	+= $(call READLIST,$(PROJECT_LISTDIR)/library-msil.lst)
LIBRARY_SOURCES		+= $(LIBRARY_LIST:%=$(LIBRARY_ROOT)/%)
LIBRARY_MSIL_SOURCES	+= $(LIBRARY_MSIL_LIST:%=$(LIBRARY_ROOT)/%)
LIBRARY_JC_FILES	+= $(filter %.java,$(LIBRARY_SOURCES))
LIBRARY_JC_FLAGS	+= $(JC_FLAGS) -scala-hack
LIBRARY_SC_FILES	+= $(filter %.scala,$(LIBRARY_SOURCES))
LIBRARY_MSIL_SC_FILES	+= $(filter %.scala,$(LIBRARY_MSIL_SOURCES))
LIBRARY_SC_BOOTCLASSPATH = $(PROJECT_OUTPUTDIR):$(PROJECT_SOURCEDIR):$(JRE_JARFILE)
LIBRARY_SDC_FLAGS	+= -windowtitle "Scala Library Documentation"
LIBRARY_SDC_FLAGS	+= -doctitle "Scala<br/>$(PROJECT_VERSION)"
LIBRARY_SDC_FILES	+= $(LIBRARY_SC_FILES)
LIBRARY_SDC_OUTPUTDIR	 = $(PROJECT_APIDOCDIR)
LIBRARY_JAR_ARCHIVE	 = $(PROJECT_LIBRARYDIR)/$(PROJECT_NAME).jar
LIBRARY_JAR_INPUTDIR	 = $(PROJECT_OUTPUTDIR)
LIBRARY_JAR_FILES	+= scala
LIBRARY_SCALAC		 = $(PROJECT_BOOTSTRAPDIR)/bin/scalac

distclean		: distclean.library
distclean.library	:
	$(RM) $(LIBRARY_JAR_ARCHIVE)

$(latest)library-jc	: $(LIBRARY_JC_FILES)
	@$(make) jc target=LIBRARY LIBRARY_JC_FILES='$(subst $$,$$$$,$?)'
	touch $@

$(latest)library-sc	: $(LIBRARY_SC_FILES)
	@$(make) sc target=LIBRARY LIBRARY_SC_FILES='$(subst $$,$$$$,$?)'
	touch $@

$(latest)library-msil-sc: $(LIBRARY_MSIL_SC_FILES)
	@$(make) sc target=LIBRARY_MSIL SC_TARGET="msil"\
	    LIBRARY_SCALAC=$(PROJECT_BINARYDIR)/scalac \
	    SC_FLAGS="-uniqid -r $(PROJECT_LIBRARYDIR) -o scalalibx -g" \
	    LIBRARY_MSIL_SC_FILES='$(subst $$,$$$$,$?)'
	touch $@

$(latest)library-sdc	: $(LIBRARY_SDC_FILES)
	@$(make) sdc target=LIBRARY
	touch $@

$(LIBRARY_JAR_ARCHIVE)	: $(LATEST)library-jc
$(LIBRARY_JAR_ARCHIVE)	: $(LATEST)library-sc
$(LIBRARY_JAR_ARCHIVE)	:
	$(MV) $(PROJECT_OUTPUTDIR)/scala/tools $(PROJECT_OUTPUTDIR)/scala.tools
	@$(make) jar target=LIBRARY
	$(MV) $(PROJECT_OUTPUTDIR)/scala.tools $(PROJECT_OUTPUTDIR)/scala/tools

##############################################################################
# Targets - scala tools - util

PROJECT_SOURCES		+= $(UTIL_SOURCES)
UTIL_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/util
UTIL_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/util.lst)
UTIL_SOURCES		+= $(UTIL_LIST:%=$(UTIL_ROOT)/%)
UTIL_JC_FILES		+= $(filter %.java,$(UTIL_SOURCES))
UTIL_SC_FILES		+= $(filter %.scala,$(UTIL_SOURCES))
UTIL_SC_BOOTCLASSPATH	 = $(LIBRARY_SC_BOOTCLASSPATH)
UTIL_SCALAC		 = $(LIBRARY_SCALAC)

$(latest)util-jc	: $(UTIL_JC_FILES)
	@$(make) jc target=UTIL UTIL_JC_FILES='$?'
	touch $@

$(latest)util-sc	: $(UTIL_SC_FILES)
	@$(make) sc target=UTIL UTIL_SC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools - compiler

PROJECT_SOURCES		+= $(SCALAC_SOURCES)
SCALAC_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalac
SCALAC_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalac.lst)
SCALAC_SOURCES		+= $(SCALAC_LIST:%=$(SCALAC_ROOT)/%)
SCALAC_JC_FILES		+= $(filter %.java,$(SCALAC_SOURCES))
SCALAC_JC_CLASSPATH	 = $(JC_CLASSPATH):$(MSIL_JARFILE):$(FJBG_JARFILE)
SCALAC_SC_FILES		+= $(filter %.scala,$(SCALAC_SOURCES))
SCALAC_SC_CLASSPATH	 = $(SCALAC_JC_CLASSPATH)
SCALAC_SC_BOOTCLASSPATH	 = $(LIBRARY_SC_BOOTCLASSPATH)
SCALAC_SCALAC		 = $(LIBRARY_SCALAC)

$(latest)scalac-jc	: $(SCALAC_JC_FILES)
	@$(make) jc target=SCALAC SCALAC_JC_FILES='$?'
	touch $@

$(latest)scalac-sc	: $(SCALAC_SC_FILES)
	@$(make) sc target=SCALAC SCALAC_SC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools - interpreter

PROJECT_SOURCES		+= $(SCALAI_SOURCES)
SCALAI_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalai
SCALAI_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalai.lst)
SCALAI_SOURCES		+= $(SCALAI_LIST:%=$(SCALAI_ROOT)/%)
SCALAI_JC_FILES		 = $(SCALAI_SOURCES)

$(latest)scalai-jc	: $(SCALAI_JC_FILES)
	@$(make) jc target=SCALAI SCALAI_JC_FILES='$?'
	touch $@

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
SCALADOC_RSRC_OUTPUTDIR	 = $(SCALADOC_ROOT:$(PROJECT_SOURCEDIR)/%=$(PROJECT_OUTPUTDIR)/%)

$(latest)scaladoc-jc	: $(SCALADOC_JC_FILES)
	@$(make) jc target=SCALADOC SCALADOC_JC_FILES='$?'
	touch $@

$(latest)scaladoc-sc	: $(SCALADOC_SC_FILES)
	@$(make) sc target=SCALADOC SCALADOC_SC_FILES='$?'
	touch $@

$(latest)scaladoc-rsrc	: $(SCALADOC_RSRC_FILES)
	$(strip $(MIRROR) -m 644 -C $(SCALADOC_ROOT) $(SCALADOC_RSRC_LIST) \
	    $(SCALADOC_RSRC_OUTPUTDIR))
	touch $@

##############################################################################
# Targets - scala tools - scalap

PROJECT_SOURCES		+= $(SCALAP_SOURCES)
SCALAP_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalap
SCALAP_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalap.lst)
SCALAP_SOURCES		+= $(SCALAP_LIST:%=$(SCALAP_ROOT)/%)
SCALAP_SC_FILES	 	+= $(SCALAP_SOURCES)

$(latest)scalap-sc	: $(SCALAP_SC_FILES)
	@$(make) sc target=SCALAP SCALAP_SC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools - dtd2scala

PROJECT_SOURCES		+= $(DTD2SCALA_SOURCES)
DTD2SCALA_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/dtd2scala
DTD2SCALA_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/dtd2scala.lst)
DTD2SCALA_SOURCES	+= $(DTD2SCALA_LIST:%=$(DTD2SCALA_ROOT)/%)
DTD2SCALA_SC_FILES	+= $(filter %.scala,$(DTD2SCALA_SOURCES))
DTD2SCALA_RSRC_LIST	+= $(filter %.xml,$(DTD2SCALA_LIST))
DTD2SCALA_RSRC_FILES	+= $(filter %.xml,$(DTD2SCALA_SOURCES))
DTD2SCALA_RSRC_OUTPUTDIR = $(DTD2SCALA_ROOT:$(PROJECT_SOURCEDIR)/%=$(PROJECT_OUTPUTDIR)/%)

$(latest)dtd2scala-sc	: $(DTD2SCALA_SC_FILES)
	@$(make) sc target=DTD2SCALA DTD2SCALA_SC_FILES='$?'
	touch $@

$(latest)dtd2scala-rsrc	: $(DTD2SCALA_RSRC_FILES)
	$(strip $(MIRROR) -m 644 -C $(DTD2SCALA_ROOT) $(DTD2SCALA_RSRC_LIST) \
	    $(DTD2SCALA_RSRC_OUTPUTDIR))
	touch $@

##############################################################################
# Targets - scala tools - scala4ant

PROJECT_SOURCES		+= $(SCALA4ANT_SOURCES)
SCALA4ANT_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scala4ant
SCALA4ANT_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scala4ant.lst)
SCALA4ANT_SOURCES	+= $(SCALA4ANT_LIST:%=$(SCALA4ANT_ROOT)/%)
SCALA4ANT_SC_FILES	+= $(SCALA4ANT_SOURCES)
SCALA4ANT_SC_CLASSPATH	 = $(SC_CLASSPATH):$(ANT_JARFILE)

$(latest)scala4ant-sc	: $(SCALA4ANT_SC_FILES)
	@$(make) sc target=SCALA4ANT SCALA4ANT_SC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools - scalatest

PROJECT_SOURCES		+= $(SCALATEST_SOURCES)
SCALATEST_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/scalatest
SCALATEST_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/scalatest.lst)
SCALATEST_SOURCES	+= $(SCALATEST_LIST:%=$(SCALATEST_ROOT)/%)
SCALATEST_JC_FILES	+= $(SCALATEST_SOURCES)

$(latest)scalatest-jc	: $(SCALATEST_JC_FILES)
	@$(make) jc target=SCALATEST SCALATEST_JC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools - servlet engine

PROJECT_SOURCES		+= $(SERVLET_SOURCES)
SERVLET_ROOT		 = $(PROJECT_SOURCEDIR)/scala/tools/servlet
SERVLET_LIST		+= $(call READLIST,$(PROJECT_LISTDIR)/servlet.lst)
SERVLET_SOURCES		+= $(SERVLET_LIST:%=$(SERVLET_ROOT)/%)
SERVLET_SC_FILES	+= $(SERVLET_SOURCES)
SERVLET_SC_CLASSPATH	 = $(PROJECT_CLASSPATH)

$(latest)servlet-sc	: $(SERVLET_SC_FILES)
	@$(make) sc target=SERVLET SERVLET_SC_FILES='$?'
	touch $@

##############################################################################
# Targets - scala tools

TOOLS_NAME		 = tools
TOOLS_JAR_ARCHIVE	 = $(PROJECT_LIBRARYDIR)/$(TOOLS_NAME).jar
TOOLS_JAR_INPUTDIR	 = $(PROJECT_OUTPUTDIR)
TOOLS_JAR_FILES		+= ch
TOOLS_JAR_FILES		+= scala/tools/dtd2scala
TOOLS_JAR_FILES		+= scala/tools/scala4ant
TOOLS_JAR_FILES		+= scala/tools/scalac
TOOLS_JAR_FILES		+= scala/tools/scaladoc
TOOLS_JAR_FILES		+= scala/tools/scalai
TOOLS_JAR_FILES		+= scala/tools/scalap
TOOLS_JAR_FILES		+= scala/tools/scalatest
TOOLS_JAR_FILES		+= scala/tools/util
TOOLS_JAR_FILES		+= scalac

distclean		: distclean.tools
distclean.tools	:
	$(RM) $(TOOLS_JAR_ARCHIVE)

$(TOOLS_JAR_ARCHIVE)	: $(LATEST)lamplib-jc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)util-jc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)util-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scalac-jc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scalac-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scalai-jc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scaladoc-jc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scaladoc-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scaladoc-rsrc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scalap-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)dtd2scala-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)dtd2scala-rsrc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scala4ant-sc
$(TOOLS_JAR_ARCHIVE)	: $(LATEST)scalatest-jc
$(TOOLS_JAR_ARCHIVE)	:
	@$(make) jar target=TOOLS

##############################################################################
# Targets - template expansion

# generation of Function<n>.java
FUNCTION_FILES		+= $(filter $(LIBRARY_ROOT)/Function%.java,$(LIBRARY_SOURCES))
FUNCTION_TEMPLATE	 = $(LIBRARY_ROOT)/Function.java.tmpl

# generation of Tuple<n>.scala
TUPLE_FILES		+= $(filter $(LIBRARY_ROOT)/Tuple%.scala,$(LIBRARY_SOURCES))
TUPLE_TEMPLATE		 = $(LIBRARY_ROOT)/Tuple.scala.tmpl

fastclean		: fastclean.generate
fastclean.generate	:
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(RM) .generated

$(FUNCTION_FILES)	: $(LATEST)meta-jc $(FUNCTION_TEMPLATE)
	@$(make) generate

$(TUPLE_FILES)		: $(LATEST)meta-jc $(TUPLE_TEMPLATE)
	@$(make) generate

%			: $(LATEST)meta-jc %.tmpl
	@$(make) generate

generate		: $(LATEST)meta-jc
	@if [ -f .generated ]; then $(call RUN,$(RM) `$(CAT) .generated`); fi
	$(strip $(JAVA) -cp $(PROJECT_CLASSPATH) \
	    meta.GenerateAll $(PROJECT_SOURCEDIR) .generated)

.PHONY			: generate

##############################################################################
# Includes

include $(PROJECT_ROOT)/Makefile.distrib
include $(PROJECT_SUPPORTDIR)/make/jc.mk
include $(PROJECT_SUPPORTDIR)/make/jar.mk
include $(PROJECT_SUPPORTDIR)/make/sc.mk
include $(PROJECT_SUPPORTDIR)/make/sdc.mk

##############################################################################
# Beta code

show-missing		:
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
