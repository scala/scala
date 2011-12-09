#!/usr/bin/env bash
#

[[ $# -gt 0 ]] || {
  echo "Usage: $0 <version> [publish destination]"
  echo ""
  exit 0
}

version="$1"
shift
rsyncDest="$1"

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
  rsync -az dists/archives/ "$rsyncDest/distributions"
  # SKIP PUBLISHING DOCS IN 2.8.X BRANCH
  if [[ $version != "2.8.x" ]]; then
    rsync -az build/scaladoc/ "$rsyncDest/docs"
  fi
  rsync -az dists/sbaz/ "$rsyncDest/sbaz"
  # Deploy the maven artifacts on scala-tools.org
  ( cd dists/maven/latest && ant deploy.snapshot -Dsettings.file="$mavenSettings" )
fi
