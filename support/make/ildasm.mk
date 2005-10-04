############################################################-*-Makefile-*-####
# ILDASM - MSIL Disassembler
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make idlasm [target=<target>] {<VARIABLE>=<value>}
#
##############################################################################
# Variables
#
# ILDASM_ASSEMBLER	 = assembler name, for example ILDASM
# $(ILDASM_ASSEMBLER)	 = assembler command
# $(ILDASM_ASSEMBLER)_FLAGS = assembler-specific compilation flags
# ILDASM_FLAGS		+= compilation flags
# ILDASM_OUTPUTFILE	 = name of the output assembly file
# ILDASM_FILES		+= files to compile
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
# Disassembly file "lib/scala.dll"
#
#   make ildasm ILDASM_FILES=lib/scala.dll
#
#
##############################################################################

##############################################################################
# Defaults

ILDASM_DISASSEMBLER	?= $(MSIL_PLATFORM)_ILDASM
CYGWIN_PATH		?= $(1)
CYGWIN_FILE		?= $(1)
MONO_ILDASM		?= monodis
MSCLR_ILDASM		?= ildasm

##############################################################################
# Values

ildasm_DISASSEMBLER	 = $(call ILDASM_LOOKUP,ILDASM_DISASSEMBLER)
ildasm_disassembler	 = $(call ILDASM_LOOKUP,$(ildasm_DISASSEMBLER))
ildasm_disassembler_flags = $(call ILDASM_LOOKUP,$(ildasm_DISASSEMBLER)_FLAGS)
ildasm_FLAGS		 = $(call ILDASM_LOOKUP,ILDASM_FLAGS)
ildasm_OUTPUTFILE	 = $(call ILDASM_LOOKUP,ILDASM_OUTPUTFILE)
ildasm_FILES		 = $(call ILDASM_LOOKUP,ILDASM_FILES)

##############################################################################

MONO_ILDASM_OUT_FLAG	 = --output=
MSCLR_ILDASM_OUT_FLAG	 = /out=

ILDASM_OUT_FLAG		 = $($(MSIL_PLATFORM)_ILDASM_OUT_FLAG)

##############################################################################
# Command

ildasm			+= $(ildasm_disassembler)
ildasm			+= $(ildasm_disassembler_flags)
ildasm			+= $(ildasm_FLAGS)
ildasm			+= $(ildasm_OUTPUTFILE:%=$(ILDASM_OUT_FLAG)$(call CYGWIN_FILE,%))
ildasm			+= $(ildasm_FILES:%=$(call CYGWIN_FILE,'%'))

##############################################################################
# Functions

ILDASM_LOOKUP		 = $(if $($(target)_$(1)),$($(target)_$(1)),$($(1)))

##############################################################################
# Rules

ildasm		:
	$(strip $(ildasm))

.PHONY		: ildasm

##############################################################################
