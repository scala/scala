#!/bin/sh

##############################################################################
# Author  : Nikolay Mihaylov
##############################################################################

##############################################################################
# variables

OBJDIR=./classes

if [ -z "${JAVA_HOME}" ]; then
  echo "environment variable JAVA_HOME is undefined."
  exit
fi

JAVAC=${JAVA_HOME}/bin/javac
JAVAC_OPTIONS="-source 1.5 -target 1.5"

JAR=${JAVA_HOME}/bin/jar

##############################################################################
# commands

mkdir -p ${OBJDIR}
${JAVAC} ${JAVAC_OPTIONS} -d ${OBJDIR} SourceAnnotation.java NestedAnnotations.java
${JAR} cf ../lib/annotations.jar -C ${OBJDIR} .
rm -rf ${OBJDIR}
