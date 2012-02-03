#!/usr/bin/env bash
#
# This rm -rfs your build dir.

set -e

run () {
  echo "% $@"
  "$@"
}

troubleFiles=$(cat <<EOM
src/library/scala/collection/generic/GenericTraversableTemplate.scala
src/library/scala/Function1.scala
EOM
)
locker=build/locker/classes/library
quick=build/quick/classes/library

run pull-binary-libs.sh
run rm -rf ./build
run mkdir -p $locker
for f in $troubleFiles; do
  run ./tools/starr_scalac -d $locker $f
done

run env ANT_OPTS="-Xmx2g -Xms2g" ant "$@" locker.done
run mkdir -p $quick

for f in $troubleFiles; do
  run ./tools/locker_scalac -d $quick $f
done

run env ANT_OPTS="-Xmx2g -Xms2g" ant "$@" build
