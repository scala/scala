############################################################-*-Makefile-*-####
# GREP - Search Regular Expressions
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make grep [FLAGS=<flags>] REGEX=<regex> [FILES=<files>]
#
##############################################################################
# Examples
#
# Search for "runtime" in all source files:
#
#   make grep REGEX=runtime
#
#
# Search for "runtime" in the compiler source files:
#
#   make grep REGEX=runtime FILES='$(COMPILER_SOURCES)'
#
##############################################################################

##############################################################################
# Defaults

GREP			?= grep

##############################################################################
# Variables

GREP_BINARY		?= $(GREP)
GREP_FLAGS		?= $(FLAGS)
GREP_REGEX		?= $(REGEX)
GREP_FILES		?= $(if $(FILES),$(FILES),$(PROJECT_SOURCES))

##############################################################################
# Rules

grep		:
	@if [ -z '$(GREP_REGEX)' ]; then \
	    echo "Usage:" \
	       "$(MAKE) $@ [FLAGS=<flags>] REGEX=<regex> [FILES=<files>]";\
	    exit 1; \
	fi
	@$(GREP_BINARY) $(GREP_FLAGS) '$(GREP_REGEX)' $(GREP_FILES:%='%')

.PHONY		: grep

##############################################################################
