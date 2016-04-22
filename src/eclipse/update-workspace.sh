#!/bin/bash

function usage() {
    echo "$0 scala_checkout_dir [workspace_dir]"
    echo "\n Add necessary path variables to Eclipse workspace settings for Scalac to build"
}

METADATA_DIR=`pwd`/.metadata

if [ $# -lt 1 ]; then
    echo "Need the Scala directory checkout as argument"
    exit 1
fi

SCALA_DIR=$1

if [ ! -z $2 ]; then
    METADATA_DIR=$2/.metadata
fi

if [ ! -d $METADATA_DIR ]; then
    echo "$METADATA_DIR is not a directory"
    exit 1
fi

echo "Using metadata directory $METADATA_DIR and Scala checkout $SCALA_DIR" 

CORE_PREFS=$METADATA_DIR/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.core.resources.prefs
if [ ! -f $CORE_PREFS ]; then
    echo "Couldn't find $CORE_PREFS. Is $METADATA_DIR an Eclipse workspace?"
    exit 1
fi
echo -e "Workspace preferences:\t$CORE_PREFS"

JDT_PREFS=$METADATA_DIR/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.core.prefs
if [ ! -f $JDT_PREFS ]; then
    echo "Couldn't find $JDT_PREFS. Creating fresh file."
    touch $JDT_PREFS
fi
echo -e "JDT preferences:\t$JDT_PREFS" 

# $1 - preference file (will be backed-up before writing)
# $2 - preference key
# $3 - preference value
function updatePref() {
    mv $1 ${1}_backup
      
    awk -v key=$2 -v value=$3 '
    BEGIN { 
        FS="="; 
        OFS="=";
        prev=""
    }
    { 
        if ($1 == key) {
            prev=$2
            $2=value
        }
        print
    }
    END { 
        if (prev) {
            printf "Updated existing value from %s to %s\n", prev, value > "/dev/stderr"
        } else {
            print key,value
        }
    }
    ' ${1}_backup >$1
}

updatePref $CORE_PREFS "pathvariable.SCALA_BASEDIR" $SCALA_DIR
updatePref $JDT_PREFS "org.eclipse.jdt.core.classpathVariable.SCALA_BASEDIR" $SCALA_DIR
