#!/bin/bash

# TRAVIS_CI_SECRET=$(cat /dev/urandom | head -c 10000 | openssl sha1)
# travis encrypt "PRIV_KEY_SECRET=$TRAVIS_CI_SECRET"

# every forced command needs a new key -- generate a pair thusly:
# ssh-keygen -b 4096 -f ~/.ssh/id_dsa_spec213_b4096
# openssl aes-256-cbc -pass "pass:$TRAVIS_CI_SECRET" -in ~/.ssh/id_dsa_spec213_b4096 -out ./spec/id_dsa_travis.enc -a

if [ "${PRIV_KEY_SECRET}" != "" -a "${TRAVIS_PULL_REQUEST}" = "false" ] ; then
  openssl aes-256-cbc -pass "pass:$PRIV_KEY_SECRET" -in spec/id_dsa_travis.enc -out spec/id_dsa_travis -d -a
  chmod 600 spec/id_dsa_travis
  eval "$(ssh-agent)"
  ssh-add -D
  ssh-add spec/id_dsa_travis
  rsync -e "ssh -o StrictHostKeyChecking=no" -rzv build/spec/ scalatest@chara.epfl.ch:/home/linuxsoft/archives/scala/spec/2.12/
fi

