#!/bin/sh
#
# This script should fail with any build of scala where #2365
# is not fixed, and otherwise succeed.  Failure means running out
# of PermGen space.

CP=.:/local/lib/java/ivy.jar
# SCALAC=/scala/inst/28/bin/scalac
SCALAC=scalac
RUN_OPTS="-XX:MaxPermSize=25M -verbose:gc"

$SCALAC -cp $CP *.scala
JAVA_OPTS="${RUN_OPTS}" scala -cp $CP Test
