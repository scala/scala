#!/bin/sh

export ANT_OPTS=-Xmx256m

awk '/^$/ {next;} /^#/ {next;} {print "export " $1 $2 $3}' build-nsc.properties > env.sh

. env.sh

# ------- don't change these, change build-nsc.properties instead

# jars for `scalac' task
CLASSPATH=${fjbg_jar}
CLASSPATH=$CLASSPATH:${scala_jar}
CLASSPATH=$CLASSPATH:${tools_jar}

# jars for `pico' task including the `-scala-hacks' enableda
CLASSPATH=$CLASSPATH:${jaco_jar}
CLASSPATH=$CLASSPATH:${hacked_pico_dir}

# jars for `nsc' task (once its compiled)
CLASSPATH=$CLASSPATH:${jars_dir}/nsc4ant.jar:${jars_dir}/nsc.jar

export CLASSPATH

# for debugging your classpath

#echo $CLASSPATH

ant -f build-nsc.xml $*
