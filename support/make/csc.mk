############################################################-*-Makefile-*-####
# CSC - Compile C# Files
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make csc [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# CSC_COMPILER		 = compiler name, for example CSC
# $(CSC_COMPILER)	 = compiler command
# $(CSC_COMPILER)_FLAGS	 = compiler-specific compilation flags
# CSC_FLAGS		+= compilation flags
# CSC_OUTPUTFILE	 = name of the generated assembly file
# CSC_REFERENCE		 = list of referenced assenblies
# CSC_ENCODING		 = character encoding used by source files
# CSC_SOURCE		 = version of source code
# CSC_TARGET		 = compilation target (exe, winexe, library, module)
# CSC_DEFINE		 = defines the symbols listed by the semicolon
#                          separated list
# CSC_FILES		+= files to compile
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
# Compile file "sources/scala/AnyVal.cs"
#
#   make csc CSC_FILES=sources/scala/AnyVal.cs
#
#
# Compile the whole runtime
#
#   make csc target=LIBRARY
#
#
# A Makefile rule that compiles all modified files from the runtime
#
#   .latest-library		: $(LIBRARY_CSC_FILES)
#           @$(MAKE) csc target=LIBRARY LIBRARY_CSC_FILES='$?'
#           touch $@
#
##############################################################################

##############################################################################
# Defaults

CSC_COMPILER		?= $(MSIL_PLATFORM)_CSC
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
MONO_CSC		?= mcs
MSCLR_CSC		?= csc

##############################################################################
# Values

csc_COMPILER		 = $(call CSC_LOOKUP,CSC_COMPILER)
csc_compiler		 = $(call CSC_LOOKUP,$(csc_COMPILER))
csc_compiler_flags	 = $(call CSC_LOOKUP,$(csc_COMPILER)_FLAGS)
csc_FLAGS		 = $(call CSC_LOOKUP,CSC_FLAGS)
csc_LIBDIR		 = $(call CSC_LOOKUP,CSC_LIBDIR)
csc_REFERENCE		 = $(call CSC_LOOKUP,CSC_REFERENCE)
csc_OUTPUTFILE		 = $(call CSC_LOOKUP,CSC_OUTPUTFILE)
csc_ENCODING		 = $(call CSC_LOOKUP,CSC_ENCODING)
csc_TARGET		 = $(call CSC_LOOKUP,CSC_TARGET)
csc_DEFINE		 = $(call CSC_LOOKUP,CSC_DEFINE)
csc_FILES		 = $(call CSC_LOOKUP,CSC_FILES)

##############################################################################
# Command

csc			+= $(csc_compiler)
csc			+= $(csc_compiler_flags)
csc			+= $(csc_FLAGS)
csc			+= $(csc_LIBDIR:%=/lib:$(call CYGWIN_PATH,%))
csc			+= $(csc_REFERENCE:%=/r:$(call CYGWIN_PATH,%))
csc			+= $(csc_OUTPUTFILE:%=/out:$(call CYGWIN_FILE,%))
csc			+= $(csc_ENCODING:%=/codepage:%)
csc			+= $(csc_TARGET:%=/target:%)
csc			+= $(csc_DEFINE:%=/define:%)
csc			+= $(csc_FILES:%=$(call CYGWIN_FILE,'%'))

##############################################################################
# Functions

CSC_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

csc:
	$(strip $(csc))

.PHONY		: csc

##############################################################################
