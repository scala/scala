############################################################-*-Makefile-*-####
# RMIC - Generate Stubs for Java Class Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make rmic [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# RMIC_COMPILER		 = compiler name, for example RMIC
# $(RMIC_COMPILER)	 = compiler command
# $(RMIC_COMPILER)_FLAGS = compiler-specific compilation flags
# RMIC_FLAGS		+= compilation flags
# RMIC_CLASSPATH	 = location of user class files
# RMIC_BOOTCLASSPATH	 = location of bootstrap class files
# RMIC_EXTDIRS		 = location of installed extensions
# RMIC_OUTPUTDIR	 = directory where to place generated class files
# RMIC_CLASSES		+= files to compile
#
# All variables may have target specific values which override the
# normal value. Those values are specified by variables whose name is
# prefixed with the target name. For example, to override the value of
# the variable RMIC_CLASSPATH with target COMPILER, one may define the
# variable COMPILER_RMIC_CLASSPATH.
#
##############################################################################
# Examples
#
# Compile file "objects/main/lib/scala/scala/runtime/distributed/ChannelImpl"
#
#   make rmic RMIC_CLASSES=objects/main/lib/scala/scala/runtime/distributed/ChannelImpl
#
#
# Compile the whole runtime
#
#   make rmic target=RUNTIME
#
#
# A Makefile rule that compiles all modified files from the runtime
#
#   .latest-runtime		: $(RUNTIME_RMIC_CLASSES)
#           @$(MAKE) rmic target=RUNTIME RUNTIME_RMIC_CLASSES='$?'
#           touch $@
#
##############################################################################

##############################################################################
# Defaults

RMIC_COMPILER		?= RMIC
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
RMIC			?= rmic

##############################################################################
# Values

rmic_COMPILER		 = $(call RMIC_LOOKUP,RMIC_COMPILER)
rmic_compiler		 = $(call RMIC_LOOKUP,$(rmic_COMPILER))
rmic_FLAGS		 = $(call RMIC_LOOKUP,RMIC_FLAGS)
rmic_CLASSPATH		 = $(call RMIC_LOOKUP,RMIC_CLASSPATH)
rmic_BOOTCLASSPATH	 = $(call RMIC_LOOKUP,RMIC_BOOTCLASSPATH)
rmic_EXTDIRS		 = $(call RMIC_LOOKUP,RMIC_EXTDIRS)
rmic_OUTPUTDIR		 = $(call RMIC_LOOKUP,RMIC_OUTPUTDIR)
rmic_CLASSES		 = $(call RMIC_LOOKUP,RMIC_CLASSES)

##############################################################################
# Command

rmic			+= $(rmic_compiler)
rmic			+= $(rmic_FLAGS)
rmic			+= $(rmic_CLASSPATH:%=-classpath $(call CYGWIN_PATH,%))
rmic			+= $(rmic_BOOTCLASSPATH:%=-bootclasspath $(call CYGWIN_PATH,%))
rmic			+= $(rmic_EXTDIRS:%=-extdirs $(call CYGWIN_PATH,%))
rmic			+= $(rmic_OUTPUTDIR:%=-d $(call CYGWIN_FILE,%))
rmic			+= $(rmic_CLASSES)

##############################################################################
# Functions

RMIC_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

rmic		:
	@[ -d "$(rmic_OUTPUTDIR)" ] || $(MKDIR) -p "$(rmic_OUTPUTDIR)"
	$(strip $(rmic))

.PHONY		: rmic

##############################################################################
