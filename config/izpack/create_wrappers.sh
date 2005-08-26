#!/bin/sh

DIRNAME="dirname"
LN="/bin/ln -sf"

COMMANDS="scala scalac scaladoc scala-info scalaint scalansc scalanstest scalap scalarun scalatest"
COMMANDS_DEBUG="scala-debug scalac-debug scaladoc-debug scalaint-debug scalansc-debug scalarun-debug"

cd `$DIRNAME $0` && \
for cmd in $COMMANDS; do $LN .scala_wrapper $cmd; done && \
for cmd in $COMMANDS_DEBUG; do $LN .scala_wrapper $cmd; done

# $Id$
