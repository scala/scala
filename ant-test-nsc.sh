#!/bin/bash
########################################################-*-Shell-script-*-####
# Test nsc
##############################################################################
# $Id$

. ant-common.sh test-nsc

##############################################################################
# ant build

CLASSPATH=$nsc_fjbg_jar:$nsc_scala_jar:$nsc_tools_jar:$nsc_nsc_jar:$nsc_nsc4ant_jar
PLATFORM=unix

# For Cygwin, switch paths to appropriate format before running ant
if $cygwin; then
    if [ "$OS" = "Windows_NT" ] && cygpath -m .>/dev/null 2>/dev/null ; then
        format=mixed
    else
        format=windows
    fi
    CLASSPATH=`cygpath --path --$format "$CLASSPATH"`
    PLATFORM=win
fi

# for debugging your classpath
#echo CLASSPATH=$CLASSPATH

CLASSPATH="$CLASSPATH" $ANT_CMD -Dplatform=$PLATFORM -f "$ANT_BUILDFILE" $*
$RM "$ANT_BUILDFILE"

##############################################################################
