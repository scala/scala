#!/bin/bash
########################################################-*-Shell-script-*-####
# Build nsc
##############################################################################
# $Id$

. ant-common.sh build-nsc

##############################################################################
# ant build

CLASSPATH=$nsc_fjbg_jar:$nsc_scala_jar:$nsc_tools_jar:$nsc_jaco_jar:$nsc_ant_jar
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
