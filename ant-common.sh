#!/bin/bash
########################################################-*-Shell-script-*-####
# Common settings
##############################################################################
# $Id$

##############################################################################
# Apache Ant

ANT_CMD=ant
#ANT_OPTS='-Xmx256m -Xms256M'
ANT_OPTS=

##############################################################################
# Shell commands

AWK=awk
CP=cp
ECHO=echo
RM='rm -f'
SED=sed

##############################################################################
# set user environment

$AWK '/^$/ {next;} /^#/ {next;} {print "export " $1 $2 $3}' \
  build-nsc.properties > env.sh

. env.sh

# ------- don't change these, change build-nsc.properties instead

function addJar() { # string -> void
  local jarfile="$1"; shift 1;
  local jarname="$1";
  if [ -f $jarfile ]; then 
    CLASSPATH=$jarfile:$CLASSPATH;
	return 0;
  else
    $ECHO you supplied $jarfile for $jarname, but it does not exists;
	return -1;
  fi
}

##############################################################################
