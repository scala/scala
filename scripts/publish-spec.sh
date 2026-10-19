#!/bin/bash

set -e

if [ -z "$CHARA_SPEC_SSH_KEY" ]; then
  echo "CHARA_SPEC_SSH_KEY is missing/empty, so skipping publish spec"
  exit
fi

# 2.12.x publishes to spec/2.12
SPEC_DIR=${GITHUB_REF_NAME%.x}
if [[ ! "$SPEC_DIR" =~ ^2\.[0-9]+$ ]]; then
  echo "not a release branch: $GITHUB_REF_NAME"
  exit 1
fi

mkdir -p ~/.ssh
echo 'chara.epfl.ch,128.178.154.107 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBLER9rqy0iCfz24+8BBBObh7FXXQJCoLLE9UuyQHNFUU4SS5FSzNjEoKXwTj8nqNy+8l0rOkj3KG8p2cLxsqjY=' >> ~/.ssh/known_hosts
(umask 077; printf '%s\n' "$CHARA_SPEC_SSH_KEY" > ~/.ssh/chara_spec)

# the key is restricted using forced commands so that it can only upload to the directory we need here.
# chara occasionally closes connections before authenticating, so retry a few times.
for attempt in 1 2 3; do
  rsync -e "ssh -i ~/.ssh/chara_spec" -rzv build/spec/ scalatest@chara.epfl.ch:/home/linuxsoft/archives/scala/spec/$SPEC_DIR/ && exit 0
  echo "rsync failed (attempt $attempt)"
  sleep 30
done
exit 1
