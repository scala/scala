#!/usr/bin/env bash
#

declare failed

a=${1:-quick}
b=${2:-strap}

echo "Comparing build/$a/classes and build/$b/classes"
for dir in library reflect compiler; do
  # feel free to replace by a more elegant approach -- don't know how
  if diff -rw -x '*.css' \
              -x '*.custom' \
              -x '*.gif' \
              -x '*.js' \
              -x '*.layout' \
              -x '*.png' \
              -x '*.properties' \
              -x '*.tmpl' \
              -x '*.tooltip' \
              -x '*.txt' \
              -x '*.xml' \
              build/{$a,$b}/classes/$dir
  then
    classes=$(find build/$a/classes/$dir -name '*.class' | wc -l)
    printf "%8s: %5d classfiles verified identical\n" $dir $classes
  else
    failed=true
  fi
done

[[ -z $failed ]] || exit 127
