#!/bin/sh
#
# This script should fail with any build of scala where #2365
# is not fixed, and otherwise succeed.  Failure means running out
# of PermGen space.
#

scalac -cp .:/local/lib/java/ivy.jar Test.scala
JAVA_OPTS="-XX:MaxPermSize=25M -verbose:gc" scalac -cp $CP Test
