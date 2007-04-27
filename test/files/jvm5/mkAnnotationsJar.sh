#!/bin/sh

OBJDIR=./classes

mkdir -p ${OBJDIR}
javac -d ${OBJDIR} -source 1.5 SourceAnnotation.java
jar cf ../lib/annotations.jar -C ${OBJDIR} .
rm -rf ${OBJDIR}
