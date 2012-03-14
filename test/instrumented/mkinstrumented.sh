#/bin/sh
# 
# Used to compile a jar with instrumented versions of certain classes.
# 

set -e

run () {
  echo "% $@"
  "$@"
}

if [ $# -ne 1 ]
then
  echo "Must provide build dir ('target' or 'build')."
  exit 1
fi

scriptDir=$(cd $(dirname $0) && pwd)

TOPDIR="$scriptDir/../.."
RUNTIME="$TOPDIR/src/library/scala/runtime"
SOURCES="$RUNTIME/BoxesRunTime.java $RUNTIME/ScalaRunTime.scala"
SCALAC=$TOPDIR/$1/pack/bin/scalac
SRC_DIR="$scriptDir/library/scala/runtime"
SCALALIB=$TOPDIR/$1/pack/lib/scala-library.jar
CLASSDIR="$scriptDir/classes"
ARTIFACT=instrumented.jar
DESTINATION="$TOPDIR/test/files/speclib"

[[ -x "$SCALAC" ]] || exit 1;

# compile it
run rm -rf $CLASSDIR && mkdir $CLASSDIR
run cp $SOURCES $SRC_DIR
( cd $SRC_DIR && run patch BoxesRunTime.java $scriptDir/boxes.patch && run patch ScalaRunTime.scala $scriptDir/srt.patch )

ORIG=$(find $SRC_DIR -name '*.orig')
[[ -z "$ORIG" ]] || rm -f $ORIG

JSOURCES=$(find $SRC_DIR -name "*.java" -print)
SOURCES=$(find $SRC_DIR -type f -print)
# echo $SOURCES
run $SCALAC -d $CLASSDIR $SOURCES
run javac -cp $SCALALIB -d $CLASSDIR $JSOURCES

# jar it up
run cd $CLASSDIR
run jar cf $ARTIFACT .
run mv -f $ARTIFACT "$DESTINATION"
echo "$(cd "$DESTINATION" && pwd)/$ARTIFACT has been created."