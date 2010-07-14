#!/bin/sh

##############################################################################
# Author  : Stephane Micheloud
##############################################################################

##############################################################################
# variables

# set any value to enable debugging output
debug=

cygwin=false;
darwin=false;
case "`uname`" in
    CYGWIN*) cygwin=true ;;
    Darwin*) darwin=true ;;
esac

CLASS_NAME=Test\$
CLASS_DIR=natives-jvm.obj

OBJ_NAME=natives
LIB_NAME=libnatives

if [ -z "${JAVA_HOME}" ]; then
  echo "environment variable JAVA_HOME is undefined."
  exit
elif $cygwin; then
  echo "Cygwin not supported (use 'mkLibNatives.bat')."
  exit
fi

JAVAH=${JAVA_HOME}/bin/javah
JAVAH_OPTIONS="-jni -force -classpath ${CLASS_DIR} -o ${OBJ_NAME}.h"

CC=gcc

if $darwin; then
  CC_OPTIONS="-c -arch ppc -arch i386 -arch x86_64"
  CC_INCLUDES="-I/System/Library/Frameworks/JavaVM.framework/Headers"
  LNK_OPTIONS="-dynamiclib -framework JavaVM"
  FULL_LIB_NAME=${LIB_NAME}.jnilib
else
  CC_OPTIONS=-c
  CC_INCLUDES="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/${OSTYPE}"
  LNK_OPTIONS="-shared -Wl,-soname,${LIB_NAME}"
  FULL_LIB_NAME=${LIB_NAME}.so
fi

##############################################################################
# commands

[ $debug ] && echo ${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}
${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}

[ $debug ] && echo ${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c
${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c

[ $debug ] && echo ${CC} ${LNK_OPTIONS} -o ${FULL_LIB_NAME} ${OBJ_NAME}.o
${CC} ${LNK_OPTIONS} -o ${FULL_LIB_NAME} ${OBJ_NAME}.o
