#!/bin/sh

export ANT_OPTS=-Xmx256m

awk '/^$/ {next;} /^#/ {next;} {print "export " $1 $2 $3}' build-nsc.properties > env.sh

. env.sh

# ------- don't change these, change build-nsc.properties instead

function addJar() { # string -> void
  if [ -f $1 ]; then 
    CLASSPATH=$1:$CLASSPATH;
	return 0;
  else
    echo you supplied $1 for $2, but it does not exists;
	return -1;
  fi
}
