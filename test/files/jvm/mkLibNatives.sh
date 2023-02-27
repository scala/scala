#!/bin/sh -e

##############################################################################
# Author  : Stephane Micheloud
##############################################################################

##############################################################################
# variables

# set any value to enable debugging output
debug=

cygwin=false;
darwin_x86=false;
darwin_arm=false;
case "`uname`" in
  CYGWIN*) cygwin=true ;;
  Darwin*) case "`uname -m`" in
    x86_64*) darwin_x86=true ;;
    arm64*) darwin_arm=true ;;
  esac
esac

CLASS_NAME=Test\$
CLASS_DIR=natives-jvm.obj

if [ ! -f "${CLASS_DIR}/${CLASS_NAME}.class" ]; then
  echo "first you need to run this within sbt:"
  echo "partest --debug test/files/jvm/natives.scala"
  exit
fi

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

if $darwin_x86; then
  # not sure if this stuff still works on current MacOS -- the
  # generated .jnilib file is already in version control and we're not
  # likely to need to generate it again, so I didn't bother to see if this
  # needs the same changes that are in the darwin_arm section below
  CC_OPTIONS="-c -arch i386 -arch x86_64"
  CC_INCLUDES="-I/System/Library/Frameworks/JavaVM.framework/Headers"
  LNK_OPTIONS="-dynamiclib -framework JavaVM"
  FULL_LIB_NAME=${LIB_NAME}-x86.jnilib
elif $darwin_arm; then
  CC_OPTIONS="-c -arch arm64"
  CC_INCLUDES="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/darwin"
  LNK_OPTIONS="-L${JAVA_HOME}/jre/lib/server -dynamiclib -ljvm"
  FULL_LIB_NAME=${LIB_NAME}-arm.jnilib
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
