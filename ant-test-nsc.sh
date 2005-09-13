#!/bin/bash
########################################################-*-Shell-script-*-####
# Test nsc
##############################################################################
# $Id$

. ant-common.sh test-nsc

##############################################################################
# ant build

CLASSPATH=$nsc_fjbg_jar:$nsc_scala_jar:$nsc_tools_jar:$nsc_nsc_jar:$nsc_nsc4ant_jar

# for debugging your classpath
#echo CLASSPATH=$CLASSPATH

CLASSPATH="$CLASSPATH" $ANT_CMD -Dplatform=unix -f "$ANT_BUILDFILE" $*
$RM "$ANT_BUILDFILE"

##############################################################################
