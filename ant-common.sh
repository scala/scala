#!/bin/bash
########################################################-*-Shell-script-*-####
# Common settings
##############################################################################
# $Id$

##############################################################################
# Apache Ant

ANT_CMD=ant
export ANT_OPTS='-Xmx256M'

##############################################################################
# Shell commands

CP=cp
ECHO=echo
RM='rm -f'
SED=sed

##############################################################################
# set user environment

$ANT_CMD -Dplatform=unix -q -f setenv-nsc.xml
. env.sh

ANT_CONFIG_BUILDFILE=$1.xml
ANT_BUILDFILE=concrete-$ANT_CONFIG_BUILDFILE
ANT_EXCLUDEFILE=developer/${USER}/$1-excludes.xml

if [ -f "$ANT_EXCLUDEFILE" ]; then
  $SED -e "s#userExcludes\ \"\"#userExcludes\ SYSTEM\ \"$ANT_EXCLUDEFILE\"#" \
    < $ANT_CONFIG_BUILDFILE > $ANT_BUILDFILE;
 else
  $CP $ANT_CONFIG_BUILDFILE $ANT_BUILDFILE;
fi

##############################################################################
