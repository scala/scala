#!/usr/bin/env bash
#
# Generates IntelliJ IDEA project files based on the checked-in samples.
#

set -e
export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
echo "About to delete .ipr and .iml files and replace with the .SAMPLE files. Press enter to continue or CTRL-C to cancel."
read

for f in "$SCRIPT_DIR"/*.SAMPLE; do
  g=${f%.SAMPLE}
  cp $f $g
done

STARR_VERSION="`cat $SCRIPT_DIR/../../versions.properties | grep 'starr.version' | awk  '{split($0,a,"="); print a[2]}'`"
sed "s/#starr-version#/$STARR_VERSION/g" $SCRIPT_DIR/scala.ipr.SAMPLE > $SCRIPT_DIR/scala.ipr
