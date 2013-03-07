#!/bin/sh
#

declare failed

echo "Comparing build/quick/classes and build/strap/classes"
for dir in library reflect compiler; do
  if diff --exclude='*.properties' -rw build/{quick,strap}/classes/$dir; then
    classes=$(find build/quick/classes/$dir -name '*.class' | wc -l)
    printf "%8s: %5d classfiles verified identical\n" $dir $classes
  else
    failed=true
  fi
done

[[ -z $failed ]] || exit 127
