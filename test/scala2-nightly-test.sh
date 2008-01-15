#!/bin/sh

[ -z "$JAVA_SDK" ] && ( echo "Variable JAVA_SDK not specified"; exit 1 )

ANDROID_HOME=/home/linuxsoft/apps/android
ANT_HOME=/home/linuxsoft/apps/ant
JAVA_HOME=/home/linuxsoft/apps/$JAVA_SDK
# uses scsh wrapper in ~/bin instead (mics)
#SCSH_HOME=/home/linuxsoft/apps/scsh-rh9
MONO_HOME=/home/linuxsoft/apps/mono
KVEM_HOME=/home/linuxsoft/apps/java-wtk

[ -d "$JAVA_HOME" ] || (echo "Invalid directory $JAVA_HOME"; exit 1 )

PATH=/usr/local/bin:/bin:/usr/bin:/usr/sbin
PATH=$ANT_HOME/bin:$JAVA_HOME/bin:$MONO_HOME/bin:$ANDROID_HOME/tools:$KVEM_HOME/bin:~/bin:$PATH

ANT_OPTS="-Xms1024M -Xmx1024M"
BUILD_DATE=`date +"%Y-%m-%d"`

OUTPUT_DIR=~/scala-nightly-test/$JAVA_SDK
TARGET_DIR=$OUTPUT_DIR/$BUILD_DATE-scala2
BUILD_DIR=$TARGET_DIR/scala/build
LATEST_DIR=$TARGET_DIR/scala/dists
NIGHTLY_DIR=/home/linuxsoft/archives/scala/nightly

[ -d "$OUTPUT_DIR" ] || mkdir -p "$OUTPUT_DIR"

JAVACMD=$JAVA_HOME/bin/java

killall -9 scshvm 2>1
killall -9 java 2>1

env PATH="$PATH" ANT_OPTS="$ANT_OPTS" JAVACMD="$JAVACMD" \
    ANDROID_HOME="$ANDROID_HOME" \
    ~/bin/scala2-nightly-test.scm $OUTPUT_DIR 

[ "$JAVA_SDK"="java-1.5" ] || exit 0

if [ -d "$LATEST_DIR" ] && [ `ls "$LATEST_DIR"/*.zip 2>1 | wc -l` -gt 0 ]; then
    (rm -rf $NIGHTLY_DIR && mkdir $NIGHTLY_DIR)
    (cd $LATEST_DIR && cp *.tgz *.zip *.md5 *.sbp *.advert $NIGHTLY_DIR)
    (cd $TARGET_DIR && cp log-scala2 $NIGHTLY_DIR)
    (cd $NIGHTLY_DIR && mkdir scala && cd scala && tar xzf ../scala-*[^sources].tgz)
fi
if [ -d "$BUILD_DIR/api-compiler" ] && [ -d "$NIGHTLY_DIR/scala/doc/scala" ]; then
    cp -r $BUILD_DIR/api-compiler $NIGHTLY_DIR/scala/doc/scala
fi

