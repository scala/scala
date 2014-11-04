#!/usr/bin/env bash
#
# Updates the .SAMPLE files with the current project files.
#

set -e
export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

echo "About to create overwrite the .ipr.SAMPLE and .iml.SAMPLE files with the current project files. Press enter to continue or CTRL-C to cancel."
read

for f in "$SCRIPT_DIR"/*.{iml,ipr}; do
  cp $f $f.SAMPLE
done

for f in "$SCRIPT_DIR"/*.SAMPLE; do
  g=${f%.SAMPLE}
  if [[ ! -f $g ]]; then
    echo "Stale sample file, deleting $f"
    rm $f
  fi
done
