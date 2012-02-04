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

doLocker () {
  run pull-binary-libs.sh  
  [[ -d $locker ]] || run mkdir -p $locker
  for f in $troubleFiles; do
    run ./tools/starr_scalac -d $locker $f
  done
  run env ANT_OPTS="-Xmx2g -Xms2g" ant "$@" locker.done
}
doQuick () {
  [[ -d $quick ]] || run mkdir -p $quick
  for f in $troubleFiles; do
    run ./tools/locker_scalac -d $quick $f
  done
  run env ANT_OPTS="-Xmx2g -Xms2g" ant "$@" build
}

case $1 in
  all.clean) run ant all.clean && shift && doLocker "$@" && doQuick "$@" ;;
  clean)     run ant clean && shift && doQuick "$@" ;;
  *)         echo "Freshening only: to all.clean or clean before build, give all.clean or clean as first arg" && doQuick "$@" ;;
esac
