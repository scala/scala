#!/usr/bin/env bash
#
# Script to push binary artifacts for scala from the remote repository.

. $(dirname $0)/tools/binary-repo-lib.sh

# TODO - Argument parsing for username/password.
pushJarFiles $(pwd) $1 $2
