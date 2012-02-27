#!/usr/bin/env bash
#
# Script to pull binary artifacts for scala from the remote repository.

# Avoid corrupting the jar cache in ~/.sbt and the ugly crash when curl is not installed 
# This affects Linux systems mostly, because wget is the default download tool and curl
# is not installed at all.
curl --version &> /dev/null
if [ $? -ne 0 ]
then
  echo ""
  echo "Please install curl to download the jar files necessary for building Scala."
  echo ""
  exit 1
fi

. $(dirname $0)/tools/binary-repo-lib.sh

# TODO - argument parsing...
pullJarFiles $(pwd)
