#!/bin/sh

DIRNAME="dirname"
LN="/bin/ln -sf"

COMMANDS="scala scalac scaladoc scalap"

cd `$DIRNAME $0` && \
for cmd in $COMMANDS; do $LN .nsc_wrapper $cmd; done

# $Id$
