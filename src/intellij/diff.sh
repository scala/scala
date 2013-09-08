#!/usr/bin/env bash
#
# Diffs the SAMPLE files against the working project config.
#
export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
for f in "$SCRIPT_DIR"/*.{iml,ipr}; do
	echo $f; diff -u $f.SAMPLE $f;
done
