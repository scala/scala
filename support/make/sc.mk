############################################################-*-Makefile-*-####
# SC - Compile Scala Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make sc [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# SC_COMPILER		 = compiler name, for example SCALAC
# $(SC_COMPILER)	 = compiler command
# $(SC_COMPILER)_FLAGS	 = compiler-specific compilation flags
# SC_FLAGS		+= compilation flags
# SC_CLASSPATH		 = location of user class files
# SC_SOURCEPATH		 = location of input source files
# SC_BOOTCLASSPATH	 = location of bootstrap class files
# SC_EXTDIRS		 = location of installed extensions
# SC_OUTPUTDIR		 = directory where to place generated class files
# SC_ENCODING		 = character encoding used by source files
# SC_SOURCE		 = version of source code
# SC_TARGET		 = version of target bytecode
# SC_FILES		+= files to compile
#
# All variables may have target specific values which override the
# normal value. Those values are specified by variables whose name is
# prefixed with the target name. For example, to override the value of
# the variable SC_CLASSPATH with target LIBRARY, one may define the
# variable LIBRARY_SC_CLASSPATH.
#
##############################################################################
# Examples
#
# Compile file "sources/scala/Predef.scala"
#
#   make sc SC_FILES=sources/scala/Predef.scala
#
#
# Compile the whole runtime
#
#   make sc target=LIBRARY
#
#
# A Makefile rule that compiles all modified files from the runtime
#
#   .latest-library		: $(LIBRARY_SC_FILES)
#           @$(MAKE) sc target=LIBRARY LIBRARY_SC_FILES='$?'
#           touch $@
#
##############################################################################

##############################################################################
# Defaults

SC_COMPILER		?= SCALAC
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
SCALAC			?= scalac

##############################################################################
# Values

sc_COMPILER		 = $(call SC_LOOKUP,SC_COMPILER)
sc_compiler		 = $(call SC_LOOKUP,$(sc_COMPILER))
sc_compiler_flags	 = $(call SC_LOOKUP,$(sc_COMPILER)_FLAGS)
sc_FLAGS		 = $(call SC_LOOKUP,SC_FLAGS)
sc_CLASSPATH		 = $(call SC_LOOKUP,SC_CLASSPATH)
sc_SOURCEPATH		 = $(call SC_LOOKUP,SC_SOURCEPATH)
sc_BOOTCLASSPATH	 = $(call SC_LOOKUP,SC_BOOTCLASSPATH)
sc_EXTDIRS		 = $(call SC_LOOKUP,SC_EXTDIRS)
sc_OUTPUTDIR		 = $(call SC_LOOKUP,SC_OUTPUTDIR)
sc_ENCODING		 = $(call SC_LOOKUP,SC_ENCODING)
sc_SOURCE		 = $(call SC_LOOKUP,SC_SOURCE)
sc_TARGET		 = $(call SC_LOOKUP,SC_TARGET)
sc_FILES		 = $(call SC_LOOKUP,SC_FILES)

##############################################################################
# Command

sc			+= $(sc_compiler)
sc			+= $(sc_compiler_flags)
sc			+= $(sc_FLAGS)
sc			+= $(sc_CLASSPATH:%=-classpath $(call CYGWIN_PATH,%))
sc			+= $(sc_SOURCEPATH:%=-sourcepath $(call CYGWIN_PATH,%))
sc			+= $(sc_BOOTCLASSPATH:%=-bootclasspath $(call CYGWIN_PATH,%))
sc			+= $(sc_EXTDIRS:%=-extdirs $(call CYGWIN_PATH,%))
sc			+= $(sc_OUTPUTDIR:%=-d $(call CYGWIN_FILE,%))
sc			+= $(sc_ENCODING:%=-encoding %)
sc			+= $(sc_SOURCE:%=-source %)
sc			+= $(sc_TARGET:%=-target:%)
sc			+= $(sc_FILES:%=$(call CYGWIN_FILE,'%'))

##############################################################################
# Functions

SC_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

sc		:
	@[ -d "$(sc_OUTPUTDIR)" ] || $(MKDIR) -p "$(sc_OUTPUTDIR)"
	$(strip $(sc))

.PHONY		: sc

##############################################################################
