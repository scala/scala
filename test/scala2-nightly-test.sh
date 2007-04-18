#!/bin/sh

[ -z "$JAVA_SDK" ] && JAVA_SDK=java
[ -z "$JAVA_VERSION" ] && JAVA_VERSION=1.5

ANT_HOME=/home/linuxsoft/apps/ant
JAVA_HOME=/home/linuxsoft/apps/$JAVA_SDK-$JAVA_VERSION
SCSH_HOME=/home/linuxsoft/apps/scsh-rh9

PATH=/usr/local/bin:/bin:/usr/bin:/usr/sbin
PATH=$ANT_HOME/bin:$JAVA_HOME/bin:$SCSH_HOME/bin:$PATH

ANT_OPTS="-Xms512M -Xmx768M"
BUILD_DATE=`date +"%Y-%m-%d"`

OUTPUT_DIR=~/scala-nightly-test/$JAVA_SDK-$JAVA_VERSION
TARGET_DIR=$OUTPUT_DIR/$BUILD_DATE-scala2
BUILD_DIR=$TARGET_DIR/scala/build
LATEST_DIR=$TARGET_DIR/scala/dists
NIGHTLY_DIR=/home/linuxsoft/archives/scala/nightly

[ -d "$OUTPUT_DIR" ] || mkdir -p "$OUTPUT_DIR"

env PATH="$PATH" ANT_OPTS="$ANT_OPTS" ~/bin/scala2-nightly-test.scm $OUTPUT_DIR 

if [ -d "$LATEST_DIR" ] && [ `ls "$LATEST_DIR"/*.zip | wc -l` -gt 0 ]; then
    (rm -rf $NIGHTLY_DIR && mkdir $NIGHTLY_DIR)
    (cd $LATEST_DIR && cp *.tgz *.zip *.md5 *.sbp *.advert $NIGHTLY_DIR)
    (cd $TARGET_DIR && cp log-scala2 $NIGHTLY_DIR)
    (cd $NIGHTLY_DIR && mkdir scala && cd scala && tar xzf ../scala-*[^sources].tgz)
fi
if [ -d "$BUILD_DIR/api-compiler" ] && [ -d "$NIGHTLY_DIR/scala/doc/scala" ]; then
    cp -r $BUILD_DIR/api-compiler $NIGHTLY_DIR/scala/doc/scala
fi
