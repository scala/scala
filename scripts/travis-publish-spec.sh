#!/bin/bash

set -e
openssl aes-256-cbc -pass "pass:$PRIV_KEY_SECRET" -in spec/id_dsa_travis.enc -out spec/id_dsa_travis -d -a
chmod 600 spec/id_dsa_travis
eval "$(ssh-agent)"
ssh-add -D
ssh-add spec/id_dsa_travis

# the key is restricted using forced commands so that it can only upload to the directory we need here
rsync -e "ssh -o StrictHostKeyChecking=no" -rzv build/spec/ \
  scalatest@chara.epfl.ch:/home/linuxsoft/archives/scala/spec/2.13/
