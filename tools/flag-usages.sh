#!/usr/bin/env bash
#
# find-flag-usages
# Paul Phillips <paulp@typesafe.com>
#
# Looks through the scala source tree for direct references to flag names.

set -e  # Good idea in almost all scripts: causes script to exit on any error.

# Would be better not to hardcode this.
flags='\b(ABSOVERRIDE|ABSTRACT|ACCESSOR|BRIDGE|BYNAMEPARAM|CAPTURED|CASE|CASEACCESSOR|CONTRAVARIANT|COVARIANT|DEFAULTINIT|DEFAULTPARAM|DEFERRED|EXISTENTIAL|EXPANDEDNAME|FINAL|IMPLCLASS|IMPLICIT|INCONSTRUCTOR|INTERFACE|JAVA|LABEL|LAZY|LIFTED|LOCAL|LOCKED|METHOD|MIXEDIN|MODULE|MODULEVAR|MUTABLE|OVERLOADED|OVERRIDE|PACKAGE|PARAM|PARAMACCESSOR|PRESUPER|PRIVATE|PROTECTED|SEALED|SPECIALIZED|STABLE|STATIC|SUPERACCESSOR|SYNTHETIC|TRAIT|TRIEDCOOKING|VARARGS|VBRIDGE)\b'

# $() runs a command in a subshell.  This is calculating the root of the
# repository by looking relative to the location of the script.
rootdir=$(cd $(dirname $0) ; pwd)/..

# A bash function.  Can be used like a command.
usage () {
  # A here string.  Allows for blocks of text without lots of quoting.
  # Variable interpolation still takes place, e.g. $(basename $0).
  cat <<EOM
Usage: $(basename $0) [-achs]
  -a show all flag usages
  -c count flag usages per file
  -h show this message
  -s count total flag usages
EOM
}

# Checking for no arguments or missing requirements.
if [[ $# -eq 0 ]]; then
  usage
  exit 1
elif [[ ! $(which ack) ]]; then   # no ack
  echo "Error: cannot find required program ack."
  exit 1
fi

# Using pushd/popd for directory changes is a way to make moving
# the current directory around somewhat more composable.
pushd "$rootdir" >/dev/null

# The leading : in :achs suppresses some errors. Each letter is a valid
# option. If an option takes an argument, a colon follows it, e.g.
# it would be :ach:s if -h took an argument.
while getopts :achs opt; do
  case $opt in
    a) ack "$flags" src ;;
    c) ack --files-with-matches -c "$flags" src ;;
    h) usage ;;
    s) ack --no-filename -o "$flags" src | sort | uniq -c | sort -gr ;;
    :) echo "Option -$OPTARG requires an argument." >&2 ;;      # this case is called for a missing option argument
    *) echo "Unrecognized argument $OPTARG" ;;                  # this is the catch-all implying an unknown option
  esac
done

# This removes all the options from $@, as getopts doesn't touch it.
# After this, "$@" contains the non-option arguments.
shift $((OPTIND-1))

# In this program we don't expect any.
if [[ $# -ne 0 ]]; then
  echo "This program does not take arguments."
fi

popd >/dev/null
exit 0
