MD_SOURCES := 01-title.md \
             02-preface.md 03-lexical-syntax.md \
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
             16-references.md

BUILD_DIR := build

.PHONY: dirs all html mathml pdf tex epub2 epub3 md clean


all: html mathml pdf tex epub2 epub3 md


clean:
	rm -rf $(BUILD_DIR)


html: $(BUILD_DIR)/ScalaReference.html


mathml: $(BUILD_DIR)/ScalaReference-mathml.html


pdf: $(BUILD_DIR)/ScalaReference.pdf


tex: $(BUILD_DIR)/ScalaReference.tex


epub2: $(BUILD_DIR)/ScalaReference.epub2


epub3: $(BUILD_DIR)/ScalaReference.epub3


md: $(BUILD_DIR)/ScalaReference.md


$(BUILD_DIR)/ScalaReference.epub2: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building EPUB2 spec to $(BUILD_DIR)/ScalaReference.epub2"
	@pandoc -f markdown \
       -t epub \
      --standalone \
      --toc \
      --chapters \
      --number-sections \
      --bibliography=Scala.bib \
      --self-contained \
      -o build/ScalaReference.epub2 \
      build/ScalaReference.md


$(BUILD_DIR)/ScalaReference.epub3: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building EPUB3 spec to $(BUILD_DIR)/ScalaReference.epub3"
	@pandoc -f markdown \
       -t epub3 \
      --standalone \
      --toc \
      --chapters \
      --number-sections \
      --bibliography=Scala.bib \
      --self-contained \
      -o build/ScalaReference.epub3 \
      build/ScalaReference.md


$(BUILD_DIR)/ScalaReference.pdf: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building PDF spec to $(BUILD_DIR)/ScalaReference.pdf"
	@pandoc -f markdown \
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

$(BUILD_DIR)/ScalaReference.tex: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building LaTeX source to $(BUILD_DIR)/ScalaReference.tex"
	@pandoc -f markdown \
      --standalone \
      --toc \
      --chapters \
      --number-sections \
      --bibliography=Scala.bib \
      --self-contained \
      --latex-engine=xelatex \
      --template=resources/scala-ref-template.latex \
      -o build/ScalaReference.tex \
      build/ScalaReference.md


$(BUILD_DIR)/ScalaReference.html: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building HTML spec to $(BUILD_DIR)/ScalaReference.html"
	@pandoc -f markdown \
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

$(BUILD_DIR)/ScalaReference-mathml.html: Scala.bib $(BUILD_DIR)/ScalaReference.md
	@echo "building HTML spec to $(BUILD_DIR)/ScalaReference-mathml.html"
	@pandoc -f markdown \
       -t html5 \
       --standalone \
       --toc \
       --chapters \
       --number-sections \
       --bibliography=Scala.bib \
       --template=resources/scala-ref-template.html5 \
       --mathml \
       -o build/ScalaReference-mathml.html \
       build/ScalaReference.md


$(BUILD_DIR)/ScalaReference.md: dirs $(MD_SOURCES)
	@echo "building Markdown source to $(BUILD_DIR)/ScalaReference.md"
	@cat $(MD_SOURCES) > $(BUILD_DIR)/ScalaReference.md


dirs: $(BUILD_DIR)


$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)
	cp -R resources $(BUILD_DIR)/resources

