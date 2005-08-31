#!/bin/bash
########################################################-*-Shell-script-*-####
# Build nsc
##############################################################################
# $Id$

. ant-common.sh

##############################################################################
# jars for `scalac' task MUST EXIST

if ! addJar $fjbg_jar fjbg_jar; then exit -1; fi
if ! addJar $scala_jar scala_jar; then
  $ECHO "try: make jar target=LIBRARY" && exit -1;
fi
if ! addJar $tools_jar tools_jar; then
  $ECHO "try: make jar target=TOOLS" && exit -1;
fi

##############################################################################
# jars for `pico' task MUST EXIST including the `-scala-hacks' enabled

if ! addJar $jaco_jar jaco_jar; then exit -1; fi 

##############################################################################
# ant build

ANT_CONFIGFILE=build-nsc.xml
ANT_BUILDFILE=concrete-$ANT_CONFIGFILE
ANT_EXCLFILE=developer/${USER}/build-nsc-excludes.xml

# for debugging your classpath
#echo $CLASSPATH
if [ -f "$ANT_EXCLFILE" ]; then
  $SED -e "s#userExcludes\ \"\"#userExcludes\ SYSTEM\ \"$ANT_EXCLFILE\"#" \
    < $ANT_CONFIGFILE > $ANT_BUILDFILE;
 else
  $CP $ANT_CONFIGFILE $ANT_BUILDFILE;
fi

CLASSPATH="$CLASSPATH" $ANT_CMD -f "$ANT_BUILDFILE" $*
#$RM "$ANT_BUILDFILE"

##############################################################################
