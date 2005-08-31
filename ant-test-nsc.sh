#!/bin/bash
########################################################-*-Shell-script-*-####
# Test nsc
##############################################################################
# $Id$

. ant-common.sh

##############################################################################
# jar with fjbg, scala runtime

if ! addJar $fjbg_jar  fjbg_jar;  then exit -1; fi
if ! addJar $tools_jar tools_jar; then exit -1; fi
if ! addJar $scala_jar scala_jar; then 
  $ECHO "try: make jar target=LIBRARY" && exit -1;
fi

##############################################################################
# jars for `nsc' task (once its compiled)

if [ ! addJar $nsc4ant_jar nsc4ant_jar ]; then
  $ECHO "try 'sh ant-build-nsc.sh build.nsc4'" && exit -1;
fi
if [ ! addJar $nsc_jar nsc_jar ]; then
  $ECHO "try 'sh ant-build-nsc.sh'" && exit -1;
fi

##############################################################################
# ant build

ANT_CONFIGFILE=test-nsc.xml
ANT_BUILDFILE=concrete-$ANT_CONFIGFILE
ANT_EXCLFILE=developer/${USER}/test-nsc-excludes.xml

# for debugging your classpath
#echo $CLASSPATH
if [ -f "$ANT_EXCLFILE" ]; then
  $SED -e "s#userExcludes\ \"\"#userExcludes\ SYSTEM\ \"$ANT_EXCLFILE\"#" < $ANT_CONFIGFILE > $ANT_BUILDFILE;
 else
  $CP $ANT_CONFIGFILE $ANT_BUILDFILE;
fi

CLASSPATH="$CLASSPATH" $ANT_CMD $ANT_OPTS -f "$ANT_BUILDFILE" $*
$RM "$ANT_BUILDFILE"

##############################################################################
