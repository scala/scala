#!/bin/sh
set -eu

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
  echo "sbt \"partest --debug test/files/jvm/natives.scala\""
  exit
fi

OBJ_NAME=natives
LIB_NAME=libnatives

if [ -z "${JAVA_HOME:-}" ]; then
  echo "environment variable JAVA_HOME is undefined."
  exit
elif $cygwin; then
  echo "Cygwin not supported (use 'mkLibNatives.bat')."
  exit
fi

JAVAH=${JAVA_HOME}/bin/javah
JAVA=${JAVA_HOME}/bin/java

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

ljavah() {
  local lclass_dir="${1}"
  local lobj_name="${2}"
  local lclass_name="${3}"
  if [ -f "${JAVAH}" ]; then
    echo "javah exists in JAVA_HOME, will be used."
    ${JAVAH} -jni -force -classpath ${lclass_dir} -o ${lobj_name}.h ${lclass_name}
  else
    echo "javah does not exist in JAVA_HOME. Wrapper for .h generation from .class filess will be downloaded and used."
    local gjavah_version=0.3.1
    local gjava=gjavah-${gjavah_version}.jar
    local asm=asm-9.1.jar
    local url="https://github.com/Glavo/gjavah/releases/download/${gjavah_version}"
     if [ ! -f "${gjava}" ]; then
       curl -k -f -L -O "${url}/${gjava}"
     fi
     if [ ! -f "${asm}" ]; then
       curl -k -f -L -O "${url}/${asm}"
     fi
     ${JAVA} -jar ${gjava} -classpath ${lclass_dir} ${lclass_name}
     mv Test__.h ${lobj_name}.h
  fi
}

##############################################################################
# commands

[ $debug ] && echo ljavah ${CLASS_DIR} ${OBJ_NAME} ${CLASS_NAME}
ljavah ${CLASS_DIR} ${OBJ_NAME} ${CLASS_NAME}

[ $debug ] && echo ${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c
${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c

[ $debug ] && echo ${CC} ${LNK_OPTIONS} -o ${FULL_LIB_NAME} ${OBJ_NAME}.o
${CC} ${LNK_OPTIONS} -o ${FULL_LIB_NAME} ${OBJ_NAME}.o
