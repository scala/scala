############################################################-*-Makefile-*-####
# SDC - Compile Scala Documentation Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make sdc [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# SDC_COMPILER		 = compiler name, for example SCALADOC
# $(SDC_COMPILER)	 = compiler command
# $(SDC_COMPILER)_FLAGS	 = compiler-specific compilation flags
# SDC_FLAGS		+= compilation flags
# SDC_CLASSPATH		 = location of user class files
# SDC_SOURCEPATH	 = location of input source files
# SDC_BOOTCLASSPATH	 = location of bootstrap class files
# SDC_EXTDIRS		 = location of installed extensions
# SDC_OUTPUTDIR		 = directory where to place generated class files
# SDC_ENCODING		 = character encoding used by source files
# SDC_SOURCE		 = version of source code
# SDC_TARGET		 = version of target bytecode
# SDC_FILES		+= files to compile
#
# All variables may have target specific values which override the
# normal value. Those values are specified by variables whose name is
# prefixed with the target name. For example, to override the value of
# the variable SDC_CLASSPATH with target LIBRARY, one may define the
# variable LIBRARY_SDC_CLASSPATH.
#
##############################################################################
# Examples
#
# Compile file "sources/scala/Predef.scala"
#
#   make sdc SDC_FILES=sources/scala/Predef.scala
#
#
# Compile the whole runtime
#
#   make sdc target=LIBRARY
#
#
# A Makefile rule that compiles all modified files from the runtime
#
#   .latest-library		: $(LIBRARY_SDC_FILES)
#           @$(MAKE) sdc target=LIBRARY LIBRARY_SDC_FILES='$?'
#           touch $@
#
##############################################################################

##############################################################################
# Defaults

SDC_COMPILER		?= SCALADOC
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
SCALADOC		?= scaladoc

##############################################################################
# Values

sdc_COMPILER		 = $(call SDC_LOOKUP,SDC_COMPILER)
sdc_compiler		 = $(call SDC_LOOKUP,$(sdc_COMPILER))
sdc_compiler_flags	 = $(call SDC_LOOKUP,$(sdc_COMPILER)_FLAGS)
sdc_FLAGS		 = $(call SDC_LOOKUP,SDC_FLAGS)
sdc_CLASSPATH		 = $(call SDC_LOOKUP,SDC_CLASSPATH)
sdc_SOURCEPATH		 = $(call SDC_LOOKUP,SDC_SOURCEPATH)
sdc_BOOTCLASSPATH	 = $(call SDC_LOOKUP,SDC_BOOTCLASSPATH)
sdc_EXTDIRS		 = $(call SDC_LOOKUP,SDC_EXTDIRS)
sdc_OUTPUTDIR		 = $(call SDC_LOOKUP,SDC_OUTPUTDIR)
sdc_ENCODING		 = $(call SDC_LOOKUP,SDC_ENCODING)
sdc_SOURCE		 = $(call SDC_LOOKUP,SDC_SOURCE)
sdc_TARGET		 = $(call SDC_LOOKUP,SDC_TARGET)
sdc_FILES		 = $(call SDC_LOOKUP,SDC_FILES)

##############################################################################
# Command

sdc			+= $(sdc_compiler)
sdc			+= $(sdc_compiler_flags)
sdc			+= $(sdc_FLAGS)
sdc			+= $(sdc_CLASSPATH:%=-classpath $(call CYGWIN_PATH,%))
sdc			+= $(sdc_SOURCEPATH:%=-sourcepath $(call CYGWIN_PATH,%))
sdc			+= $(sdc_BOOTCLASSPATH:%=-bootclasspath $(call CYGWIN_PATH,%))
sdc			+= $(sdc_EXTDIRS:%=-extdirs $(call CYGWIN_PATH,%))
sdc			+= $(sdc_OUTPUTDIR:%=-d $(call CYGWIN_FILE,%))
sdc			+= $(sdc_ENCODING:%=-encoding %)
sdc			+= $(sdc_SOURCE:%=-source %)
sdc			+= $(sdc_TARGET:%=-target %)
sdc			+= $(sdc_FILES:%=$(call CYGWIN_FILE,'%'))
sdc			+= -- scala

##############################################################################
# Functions

SDC_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

sdc		:
	@[ -d "$(sdc_OUTPUTDIR)" ] || $(MKDIR) -p "$(sdc_OUTPUTDIR)"
	$(strip $(sdc))

.PHONY		: sdc

##############################################################################
