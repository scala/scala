#!/bin/bash
########################################################-*-Shell-script-*-####
# Build nsc
##############################################################################
# $Id$

. ant-common.sh build-nsc

##############################################################################
# ant build

CLASSPATH=$nsc_fjbg_jar:$nsc_scala_jar:$nsc_tools_jar:$nsc_jaco_jar

# for debugging your classpath
#echo CLASSPATH=$CLASSPATH

CLASSPATH="$CLASSPATH" $ANT_CMD -Dplatform=unix -f "$ANT_BUILDFILE" $*
$RM "$ANT_BUILDFILE"

##############################################################################
