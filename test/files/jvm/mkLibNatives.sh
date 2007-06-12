#!/bin/sh

##############################################################################
# Author  : Stephane Micheloud
# Revision: $Id: $
##############################################################################

##############################################################################
# variables

# set any value to enable debugging output
debug=1

cygwin=false;
darwin=false;
case "`uname`" in
    CYGWIN*) cygwin=true ;;
    Darwin*) darwin=true ;;
esac

CLASS_NAME=Test\$
CLASS_DIR=natives-jvm

OBJ_NAME=natives
LIB_NAME=libnatives

if [ -z "${JAVA_HOME}" ]; then
  echo "environment variable JAVA_HOME is undefined."
  exit
elif [ $cygwin ]; then
  echo "Cygwin not supported (use 'mkLibNatives.bat')."
  exit
fi

JAVAH=${JAVA_HOME}/bin/javah
JAVAH_OPTIONS="-jni -force -classpath ${CLASS_DIR} -o ${OBJ_NAME}.h"

CC=gcc
CC_OPTIONS=-c
CC_INCLUDES="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/${OSTYPE}"

LNK_OPTIONS="-shared -Wl,-soname,${LIB_NAME}"

##############################################################################
# commands

[ "$debug" ] && echo ${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}
${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}

[ "$debug" ] && echo ${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c
${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c

[ "$debug" ] && echo ${CC} -shared -Wl,-soname,${LIB_NAME} -o ${LIB_NAME}.so ${OBJ_NAME}.o
${CC} ${LNK_OPTIONS} -o ${LIB_NAME}.so ${OBJ_NAME}.o
