############################################################-*-Makefile-*-####
# DEFAULT - Default Rules
##############################################################################
# $Id$

##############################################################################
# Commands

all			: default
force			: default.force
clean			: default.clean
distclean		: default.distclean

default			:

default.force		:

default.clean		:
	$(RM) core *~

default.distclean	: clean


.PHONY			: all
.PHONY			: force
.PHONY			: clean
.PHONY			: distclean
.PHONY			: default
.PHONY			: default.force
.PHONY			: default.clean
.PHONY			: default.distclean

##############################################################################
