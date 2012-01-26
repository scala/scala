#!/usr/bin/env bash
#
# Script to push binary artifacts for scala from the remote repository.

. $(dirname $0)/tools/binary-repo-lib.sh

if test $# -lt 2; then
  echo "Usage: $0 <username> <password>"
  exit 1
fi

# TODO - Argument parsing for username/password.
pushJarFiles $(pwd) $1 $2
