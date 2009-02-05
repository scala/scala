#!/bin/bash

SCALAC="../../../../build/pack/bin/scalac -deprecation -cp ../../../../build/quick/classes/compiler/"

BASE=`pwd`

if [[ -d "${BASE}/src" ]] ; then

    mkdir -p build
    ${SCALAC} -d build src/*.scala
    jar cf lib/plugins.jar -C misc/ scalac-plugin.xml -C build .
    rm -rf build
fi

