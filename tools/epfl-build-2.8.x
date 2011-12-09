#!/bin/sh -e
#
# Jenkins should run tools/$0 --publish "$ssh_conn:$nightly_dir"

unset rsyncDest
if [ "$1" == "--publish" ]; then
  rsyncDest="$2"
fi

# should not be hardcoded
mavenSettings="/home/linuxsoft/apps/hudson-maven-settings/settings.xml"

# main build sequence
ant all.clean
./pull-binary-libs.sh
ant nightly
ant docscomp

# publish nightly build
if [ -n "$rsyncDest" ]; then
  echo "Copying nightly build to $rsyncDest"
  # Archive Scala nightly distribution
  # Tailing slash is required, otherwise the directory gets synchronized instead of its content
  rsync -az dists/archives/ "$rsyncDest/distributions"
  # SKIP PUBLISHING DOCS IN 2.8.X BRANCH
  # rsync -az scala/build/scaladoc/ "$rsyncDest/docs"
  rsync -az dists/sbaz/ "$rsyncDest/sbaz"
  # Deploy the maven artifacts on scala-tools.org
  ( cd dists/maven/latest && ant deploy.snapshot -Dsettings.file="$mavenSettings" )
fi
