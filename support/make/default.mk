############################################################-*-Makefile-*-####
# DEFAULT - Default Rules
##############################################################################
# $Id$

##############################################################################
# Commands

all			: default
clean			: default.clean
distclean		: default.distclean

default			:

default.clean		:
	$(RM) core *~

default.distclean	: default.clean


.PHONY			: all
.PHONY			: clean
.PHONY			: distclean
.PHONY			: default
.PHONY			: default.clean
.PHONY			: default.distclean

##############################################################################
