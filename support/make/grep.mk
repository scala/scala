############################################################-*-Makefile-*-####
# GREP - search regular expressions
##############################################################################
# $Id$

##############################################################################
# Usage
#
#   make grep [FLAGS=<flags>] REGEXP=<regexp> [FILES=<files>]"
#
##############################################################################
# Examples
#
# Search for "runtime" in all source files:
#
#   make grep REGEXP=runtime
#
#
# Search for "runtime" in the compiler source files:
#
#   make grep REGEXP=runtime FILES=\$\(COMPILER_SOURCES\)
#
##############################################################################

##############################################################################
# Variables

GREP_BINARY		?= $(GREP)
GREP_FLAGS		?= $(FLAGS)
GREP_REGEXP		?= $(REGEXP)
GREP_FILES		?= $(if $(FILES),$(FILES),$(PROJECT_SOURCES))

##############################################################################
# Rules

grep		:
	@if [ -z '$(GREP_REGEXP)' ]; then \
	    $(ECHO) "Usage:" \
	       "$(MAKE) grep [FLAGS=<flags>] REGEXP=<regexp> [FILES=<files>]";\
	    exit 1; \
	fi
	@$(GREP_BINARY) $(GREP_FLAGS) '$(GREP_REGEXP)' $(GREP_FILES)

.PHONY		: grep

##############################################################################
