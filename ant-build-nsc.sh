#!/bin/sh

export ANT_OPTS=-Xmx256m

CLASSPATH=/home/linuxsoft/apps/fjbg/jars/fjbg.jar
CLASSPATH=$CLASSPATH:/home/linuxsoft/apps/jaco/lib/jaco.jar
CLASSPATH=$CLASSPATH:/home/linuxsoft/apps/fjbg/jars/fjbg.jar
CLASSPATH=$CLASSPATH:/localhome/buraq/scala/objects/main/lib/scala.jar
CLASSPATH=$CLASSPATH:/localhome/buraq/scala/objects/main/lib/tools.jar
CLASSPATH=$CLASSPATH:/home/linuxsoft/apps/jaco/lib/jaco.jar
CLASSPATH=$CLASSPATH:/tmp/hackedPico
CLASSPATH=$CLASSPATH:/tmp/jars/nsc4ant.jar:/tmp/jars/nsc.jar


export CLASSPATH

ant -f build-nsc.xml $*
