#!/bin/bash
########################################################-*-Shell-script-*-####
# Build nsc
##############################################################################
# $Id$

. ant-common.sh

##############################################################################
# ant build

ANT_CONFIG_BUILDFILE=build-nsc.xml
ANT_BUILDFILE=concrete-$ANT_CONFIG_BUILDFILE
ANT_EXCLUDEFILE=developer/${USER}/build-nsc-excludes.xml

if [ -f "$ANT_EXCLUDEFILE" ]; then
  $SED -e "s#userExcludes\ \"\"#userExcludes\ SYSTEM\ \"$ANT_EXCLUDEFILE\"#" \
    < $ANT_CONFIG_BUILDFILE > $ANT_BUILDFILE;
 else
  $CP $ANT_CONFIG_BUILDFILE $ANT_BUILDFILE;
fi

CLASSPATH=$nsc_fjbg_jar:$nsc_scala_jar:$nsc_tools_jar:$nsc_jaco_jar

# for debugging your classpath
#echo CLASSPATH=$CLASSPATH

CLASSPATH="$CLASSPATH" $ANT_CMD -Dplatform=unix -f "$ANT_BUILDFILE" $*
$RM "$ANT_BUILDFILE"

##############################################################################
