#!/usr/bin/env bash
# Script copied from https://github.com/scalameta/scalameta
set -e

if [ "$TRAVIS_PULL_REQUEST" != "false" ] ; then
  echo "Incoming pull request from https://github.com/$TRAVIS_REPO_SLUG/pull/$TRAVIS_PULL_REQUEST";
  author=$(curl -u dummy4dummy:dummy2dummy -s "https://api.github.com/repos/$TRAVIS_REPO_SLUG/pulls/$TRAVIS_PULL_REQUEST" | jq -r ".user.login");
  if [ $? -ne 0 ] ; then exit 1; fi;
  echo "Pull request submitted by $author";
  signed=$(curl -s https://www.lightbend.com/contribute/cla/scala/check/$author | jq -r ".signed");
  if [ $? -ne 0 ] ; then exit 1; fi;
  if [ "$signed" = "true" ] ; then
    echo "CLA check for $author successful";
  else
    echo "CLA check for $author failed";
    echo "Please sign the Scala CLA to contribute to $TRAVIS_REPO_SLUG";
    echo "Go to https://www.lightbend.com/contribute/cla/scala and then resubmit this pull request";
    exit 1;
  fi;
fi;