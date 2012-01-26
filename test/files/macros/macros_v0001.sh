#!/bin/bash
set -o errexit

if [[ $(uname -s) == CYGWIN* ]]; then cpsep=";"; else cpsep=":"; fi
scripthome="$(dirname "$0")"
scalahome="$scripthome/../../.."
scaladeps="$scalahome/lib/jline.jar;$scalahome/lib/fjbg.jar"
scalalib="$scalahome/build/pack/lib/scala-library.jar"
if [ ! -f "$scalalib" ]; then scalalib="$scalahome/build/locker/classes/library"; fi
scalacomp="$scalahome/build/pack/lib/scala-compiler.jar"
if [ ! -f "$scalacomp" ]; then scalacomp="$scalahome/build/locker/classes/compiler"; fi
stdcp="$scaladeps$cpsep$scalalib$cpsep$scalacomp"
function scalac { java -cp "$cp" -Dscala.usejavacp=true scala.tools.nsc.Main $*; }
function scala { java -cp "$cp" -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner $*; }

echo "Compiling macros..."
cp="$stdcp"
scalac -Xmacros "$scripthome/Printf.scala"

echo "Compiling the program..."
cp="$stdcp$cpsep$scripthome"
scalac "$scripthome/Test.scala"

echo ""
echo "NOW LOOK"
echo "==============================================="
cp="$stdcp$cpsep$scripthome"
scala Test
echo ""
echo "==============================================="
