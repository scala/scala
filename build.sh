#!/bin/sh
find . -name "*.md" | \
cat 01-title.md \
    02-preface.md \
    03-lexical-syntax.md \
    04-identifiers-names-and-scopes.md \
    05-types.md > build/ScalaReference.md 
#    06-basic-declarations-and-definitions.md \
#    07-classes-and-objects.md \
#    08-expressions.md \
#    09-implicit-parameters-and-views.md \
#    10-pattern-matching.md \
#    11-top-level-definitions.md \
#    12-xml-expressions-and-patterns.md \
#    13-user-defined-annotations.md \
#    14-the-scala-standard-library.md \
#    15-scala-syntax-summary.md \

pandoc -f markdown \
       -t html5 \
       --standalone \
       --toc \
       --chapters \
       --number-sections \
       --bibliography=Scala.bib \
       --template=resources/scala-ref-template.html5 \
       --mathjax \
       -o build/ScalaReference.html \
       build/ScalaReference.md

cp -Rf resources build/

# pdf generation - not working yet
#pandoc -f markdown \
#       --standalone \
#       --toc \
#       --chapters \
#       --number-sections \
#       --bibliography=Scala.bib \
#       --self-contained \
#       --latex-engine=xelatex \
#       -o build/ScalaReference.pdf \
#       build/ScalaReference.panmd