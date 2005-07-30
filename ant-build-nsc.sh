#!/bin/sh

. ant-common.sh

# jars for `scalac' task MUST EXIST
if ! addJar $fjbg_jar fjbg_jar;  then exit -1; fi
if ! addJar $scala_jar scala_jar; then echo "try: make jar target=LIBRARY" && exit -1; fi
if ! addJar $tools_jar tools_jar; then echo "try: make jar target=TOOLS" && exit -1; fi

# jars for `pico' task MUST EXIST including the `-scala-hacks' enabled
if ! addJar $jaco_jar jaco_jar;  then exit -1; fi 

# jars for `nsc' task (once its compiled)
if ! addJar $jars_dir/nsc4ant.jar;   then echo "{ *** WARNING: $jars_dir/nsc4ant.jar doesn't exist}"; fi
if ! addJar $jars_dir/nsc.jar;       then echo "{ *** WARNING: $jars_dir/nsc.jar doesn't exist}"; fi

export CLASSPATH

# for debugging your classpath

#echo $CLASSPATH

ant -f build-nsc.xml $*
