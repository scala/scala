#!/usr/bin/env bash

# NOTES:
# "toc"                     -> treated just like another page, its location can be changed
# "--window-status loaded"  -> when window.status is set to "loaded", wkhtmltopdf knows js is loaded

ROOT_DIR=..
SPEC_SRC_DIR=$ROOT_DIR/spec
SPEC_BUILD_DIR=$ROOT_DIR/build/spec

WKHTML_OPTS="--print-media-type --window-status loaded --footer-center [page]"
WKHTML_TOC="toc --xsl-style-sheet $SPEC_SRC_DIR/spec-toc.xslt"

# exclude index.html, prepend SPEC_BUILD_DIR path
HTML_FILES=$(ls $SPEC_BUILD_DIR -I 'index.html' | grep '\.html$' | while read line; do echo "$SPEC_BUILD_DIR/$line"; done)

echo "Making Spec.pdf with HTML files: "
echo $SPEC_BUILD_DIR/index.html $HTML_FILES

# first goes index.html, then TOC, then rest
wkhtmltopdf $WKHTML_OPTS $SPEC_BUILD_DIR/index.html $WKHTML_TOC $HTML_FILES $SPEC_BUILD_DIR/Spec.pdf