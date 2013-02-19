#!/bin/sh

if [ ! -d build ]; then
    mkdir -p build
fi

echo "building Markdown source to build/ScalaReference.md"
cat 01-title.md \
    02-preface.md \
    03-lexical-syntax.md \
    04-identifiers-names-and-scopes.md \
    05-types.md \
    06-basic-declarations-and-definitions.md \
    07-classes-and-objects.md \
    08-expressions.md \
    09-implicit-parameters-and-views.md \
    10-pattern-matching.md \
    11-top-level-definitions.md \
    12-xml-expressions-and-patterns.md \
    13-user-defined-annotations.md \
    14-the-scala-standard-library.md \
    15-scala-syntax-summary.md \
    16-references.md > build/ScalaReference.md

echo "building HTML spec to build/ScalaReference.html"
pandoc -f markdown \
       -t html5 \
       --standalone \
       --toc \
       --chapters \
       --number-sections \
       --bibliography=Scala.bib \
       --template=resources/scala-ref-template.html5 \
       --mathjax='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
       -o build/ScalaReference.html \
       build/ScalaReference.md

cp -Rf resources build/

echo "building PDF spec to build/ScalaReference.pdf"
pandoc -f markdown \
      --standalone \
      --toc \
      --chapters \
      --number-sections \
      --bibliography=Scala.bib \
      --self-contained \
      --latex-engine=xelatex \
      --template=resources/scala-ref-template.latex \
      -o build/ScalaReference.pdf \
      build/ScalaReference.md

echo "building ebook to build/ScalaReference.epub"
pandoc -f markdown \
       -t epub \
      --standalone \
      --toc \
      --chapters \
      --number-sections \
      --bibliography=Scala.bib \
      --self-contained \
      -o build/ScalaReference.epub \
      build/ScalaReference.md