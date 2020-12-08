#!/usr/bin/env bash

set -e
set -v

# NOTES:
# "toc"                     -> treated just like another page, its location can be changed
# "--window-status loaded"  -> when window.status is set to "loaded", wkhtmltopdf knows js is loaded

THIS_SCRIPT_DIR=$(dirname $0)
ROOT_DIR=$THIS_SCRIPT_DIR/..
SPEC_SRC_DIR=$ROOT_DIR/spec
SPEC_BUILD_DIR=$ROOT_DIR/build/spec
SPEC_PDF_BUILD_DIR=$ROOT_DIR/build/spec-pdf
PDF=$SPEC_BUILD_DIR/spec.pdf

mkdir -p $SPEC_PDF_BUILD_DIR


WKHTML_OPTS='--print-media-type --window-status loaded --javascript-delay 1000 --load-error-handling ignore --enable-local-file-access  --footer-center [page] --footer-font-name "Luxi Sans"'
WKHTML_TOC="toc --xsl-style-sheet $SPEC_SRC_DIR/spec-toc.xslt"

# exclude index.html, prepend SPEC_PDF_BUILD_DIR path
HTML_FILES=$(ls $SPEC_PDF_BUILD_DIR | grep -vx 'index.html' | grep '\.html$' | while read line; do echo "$SPEC_PDF_BUILD_DIR/$line"; done)

echo "Making spec.pdf with HTML files: "
echo $SPEC_PDF_BUILD_DIR/index.html $HTML_FILES

# first goes index.html, then TOC, then rest
rm -f $PDF
wkhtmltopdf $WKHTML_OPTS $SPEC_PDF_BUILD_DIR/index.html $WKHTML_TOC $HTML_FILES $PDF || true

# the '|| true' thing is because we get:
#   Error: Failed to load http:/, with network status code 3 and http status code 0 - Host  not found
#   Warning: Failed loading page http: (ignored)
# as long we have `--load-error-handling ignore` we still get a PDF, but we also get a nonzero exit code

# fail if we didn't get a PDF file out
if [ ! -f $PDF ] ; then exit 1 ; fi
