#!/bin/bash -e

if [ -z "$PGP_SECRET" ]; then
  echo "PGP_SECRET is missing/empty, so skipping credentials & gpg setup"
  exit
fi

# PGP_SECRET is the base64-encoded, ASCII-armored private key, as for sbt-ci-release
sensitive() {
  envsubst < files/credentials-private-repo-netrc > ~/.credentials-private-repo-netrc
  base64 --decode <<< "$PGP_SECRET" | \
    gpg --batch --yes --import --pinentry-mode loopback --passphrase "$PGP_PASSPHRASE"
}

# don't let anything escape from the sensitive part (e.g. leak environment var by echoing to log on failure)
sensitive >/dev/null 2>&1

# just to verify
gpg --list-keys
gpg --list-secret-keys

mkdir -p ~/.sbt/1.0/plugins
cp files/gpg.sbt ~/.sbt/1.0/plugins/
