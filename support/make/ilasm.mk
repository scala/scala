############################################################-*-Makefile-*-####
# ILASM - MSIL Assembler
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make ilasm [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# ILASM_ASSEMBLER	 = assembler name, for example ILASM
# $(ILASM_ASSEMBLER)	 = assembler command
# $(ILASM_ASSEMBLER)_FLAGS = assembler-specific compilation flags
# ILASM_FLAGS		+= compilation flags
# ILASM_OUTPUTFILE	 = name of the output assembly file
# ILASM_KEYFILE		 = key file
# ILASM_FILES		+= files to compile
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
# Compile file "scala.il"
#
#   make ilasm ILASM_FILES=scala.il
#
#
##############################################################################

##############################################################################
# Defaults

ILASM_ASSEMBLER		?= $(MSIL_PLATFORM)_ILASM
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
MONO_ILASM		?= ilasm
MSCLR_ILASM		?= ilasm

##############################################################################
# Values

ilasm_ASSEMBLER		 = $(call ILASM_LOOKUP,ILASM_ASSEMBLER)
ilasm_assembler		 = $(call ILASM_LOOKUP,$(ilasm_ASSEMBLER))
ilasm_assembler_flags	 = $(call ILASM_LOOKUP,$(ilasm_ASSEMBLER)_FLAGS)
ilasm_FLAGS		 = $(call ILASM_LOOKUP,ILASM_FLAGS)
ilasm_OUTPUTFILE	 = $(call ILASM_LOOKUP,ILASM_OUTPUTFILE)
ilasm_KEYFILE		 = $(call ILASM_LOOKUP,ILASM_KEYFILE)
ilasm_FILES		 = $(call ILASM_LOOKUP,ILASM_FILES)


##############################################################################

MONO_OUT_FLAG		 = /output:
MONO_KEY_FLAG		 = /key:

MSCLR_OUT_FLAG		 = /out=
MSCLR_KEY_FLAG		 = /key=

OUT_FLAG		 = $($(MSIL_PLATFORM)_OUT_FLAG)
KEY_FLAG		 = $($(MSIL_PLATFORM)_KEY_FLAG)

##############################################################################
# Command

ilasm			+= $(ilasm_assembler)
ilasm			+= $(ilasm_assembler_flags)
ilasm			+= $(ilasm_FLAGS)
ilasm			+= $(ilasm_OUTPUTFILE:%=$(OUT_FLAG)$(call CYGWIN_FILE,%))
ilasm			+= $(ilasm_KEYFILE:%=$(KEY_FLAG)$(call CYGWIN_FILE,%))
ilasm			+= $(ilasm_FILES:%=$(call CYGWIN_FILE,'%'))

##############################################################################
# Functions

ILASM_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

ilasm		:
	$(strip $(ilasm))

.PHONY		: ilasm

##############################################################################
