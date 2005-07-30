#!/bin/sh

. ant-common.sh

# jars for `scalac' task MUST EXIST
if ! addJar $fjbg_jar fjbg_jar;  then exit -1; fi
if ! addJar $scala_jar scala_jar; then echo "try: make jar target=LIBRARY" && exit -1; fi
if ! addJar $tools_jar tools_jar; then echo "try: make jar target=TOOLS" && exit -1; fi

# jars for `pico' task MUST EXIST including the `-scala-hacks' enabled
if ! addJar $jaco_jar jaco_jar;  then exit -1; fi 

export CLASSPATH

# for debugging your classpath
#echo $CLASSPATH
if [ -f developer/${USER}/build-nsc-excludes.xml ]; then
  sed -e "s/userExcludes\ \"\"/userExcludes\ SYSTEM\ \"developer\/${USER}\/build-nsc-excludes.xml\"/" < build-nsc.xml > concrete-build-nsc.xml;
 else
  ln -s build-nsc.xml concrete-build-nsc.xml;
fi

ant -f concrete-build-nsc.xml $*
rm -f concrete-build-nsc.xml
