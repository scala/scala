#!/bin/bash -e

sensitive() {
  perl -p -e 's/\$\{([^}]+)\}/defined $ENV{$1} ? $ENV{$1} : $&/eg' < files/credentials-private-repo > ~/.credentials-private-repo
  perl -p -e 's/\$\{([^}]+)\}/defined $ENV{$1} ? $ENV{$1} : $&/eg' < files/credentials-sonatype     > ~/.credentials-sonatype
  perl -p -e 's/\$\{([^}]+)\}/defined $ENV{$1} ? $ENV{$1} : $&/eg' < files/sonatype-curl            > ~/.sonatype-curl

  openssl aes-256-cbc -d -pass "pass:$GPG_SUBKEY_SECRET" -in files/gpg_subkey.enc | gpg --import
}

# don't let anything escape from the sensitive part (e.g. leak environment var by echoing to log on failure)
sensitive >/dev/null 2>&1

# just to verify
gpg --list-keys
gpg --list-secret-keys

mkdir -p ~/.sbt/1.0/plugins
cp files/gpg.sbt ~/.sbt/1.0/plugins/
