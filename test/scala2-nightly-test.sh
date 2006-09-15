#!/bin/sh

if [ -z "$JAVA_VERSION" ]; then
    JAVA_VERSION=1.5
fi

ANT_HOME=/home/linuxsoft/apps/ant
JAVA_HOME=/home/linuxsoft/apps/java-$JAVA_VERSION
SCSH_HOME=/home/linuxsoft/apps/scsh-rh9

PATH=/usr/local/bin:/bin:/usr/bin:/usr/sbin
PATH=$ANT_HOME/bin:$JAVA_HOME/bin:$SCSH_HOME/bin:$PATH

ANT_OPTS="-Xms256M -Xmx512M"
BUILD_DATE=`date +"%Y-%m-%d"`

OUTPUT_DIR=~/scala-nightly-test/java-$JAVA_VERSION
BUILD_DIR=$OUTPUT_DIR/$BUILD_DATE-scala2
LATEST_DIR=$BUILD_DIR/scala/dists
NIGHTLY_DIR=/home/linuxsoft/archives/scala/nightly

env PATH="$PATH" ANT_OPTS="$ANT_OPTS" ~/bin/scala2-nightly-test.scm $OUTPUT_DIR 

if [ -d "$LATEST_DIR" ] && [ `ls "$LATEST_DIR"/*.zip | wc -l` -gt 0 ]; then
    (rm -rf $NIGHTLY_DIR && mkdir $NIGHTLY_DIR)
    (cd $LATEST_DIR && cp *.tgz *.zip *.md5 *.sbp *.advert $NIGHTLY_DIR)
    (cd $BUILD_DIR && cp log-scala2 $NIGHTLY_DIR)
fi
