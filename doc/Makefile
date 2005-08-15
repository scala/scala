############################################################-*-Makefile-*-####
# Scala documentation
##############################################################################
# $Id$


##############################################################################

SUBDIRS		 += introduction
SUBDIRS		 += papers
SUBDIRS		 += reference
SUBDIRS		 += tutorial

MAKE		  = make

##############################################################################
# Commands

all:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) clean -C $$dir; \
	done

distclean:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) distclean -C $$dir; \
	done

.PHONY: all
.PHONY: clean
.PHONY: distclean

##############################################################################
