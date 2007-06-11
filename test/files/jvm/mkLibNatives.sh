#!/bin/sh

CLASS_NAME=Test\$
CLASS_DIR=natives-jvm

OBJ_NAME=natives
LIB_NAME=libnatives

JAVAH=javah
JAVAH_OPTIONS="-jni -force -classpath ${CLASS_DIR} -o ${OBJ_NAME}.h"

CC=gcc
CC_OPTIONS=-c
CC_INCLUDES="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/${OSTYPE}"

#echo ${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}
#${JAVAH} ${JAVAH_OPTIONS} ${CLASS_NAME}

#echo ${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c
${CC} ${CC_OPTIONS} ${CC_INCLUDES} -o ${OBJ_NAME}.o natives.c

#echo ${CC} -shared -Wl,-soname,${LIB_NAME} -o ${LIB_NAME}.so ${OBJ_NAME}.o
${CC} -shared -Wl,-soname,${LIB_NAME} -o ${LIB_NAME}.so ${OBJ_NAME}.o
