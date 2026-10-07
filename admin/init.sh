#!/bin/bash -e

if [ -z "$GPG_ENC_PASS" ]; then
  echo "GPG_ENC_PASS is missing/empty, so skipping credentials & gpg setup"
  exit
fi

sensitive() {
  envsubst < files/credentials-private-repo-netrc > ~/.credentials-private-repo-netrc
  openssl enc -d -aes-256-cbc -salt -pbkdf2 -pass "pass:$GPG_ENC_PASS" -in files/scala-key.asc.enc | \
    gpg --batch --yes --import --pinentry-mode loopback --passphrase "$PGP_PASSPHRASE"
}

# don't let anything escape from the sensitive part (e.g. leak environment var by echoing to log on failure)
sensitive >/dev/null 2>&1

# just to verify
gpg --list-keys
gpg --list-secret-keys

mkdir -p ~/.sbt/1.0/plugins
cp files/gpg.sbt ~/.sbt/1.0/plugins/
