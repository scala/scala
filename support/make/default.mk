############################################################-*-Makefile-*-####
# DEFAULT - Default Rules
##############################################################################
# $Id$

##############################################################################
# Commands

all			: default
force			: default.force
fastclean		: default.fastclean
clean			: default.clean
distclean		: default.distclean

default			:

default.force		: fastclean

default.fastclean	:

default.clean		: fastclean

default.distclean	: clean
	$(FIND) '(' -name '*~' -o -name core ')' -exec $(RM) '{}' ';'

.PHONY			: all
.PHONY			: force
.PHONY			: fastclean
.PHONY			: clean
.PHONY			: distclean
.PHONY			: default
.PHONY			: default.force
.PHONY			: default.fastclean
.PHONY			: default.clean
.PHONY			: default.distclean

##############################################################################
