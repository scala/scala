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
