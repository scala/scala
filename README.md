# Scala Language Reference as Pandoc Markdown - Notes

## Prerequisites

In order to build the scala reference, you will require the following
software packages:

- Pandoc v1.11.1 or higher (<http://johnmacfarlane.net/pandoc/>)
  you can install this using cabal:

      ```
      cabal update
      cabal install pandoc
      ```

- TeX-Live (<https://www.tug.org/texlive/>), in order to build the pdf
version of the specification.

- The luximono font - this does not ship with TeX-Live by default due to
  license restrictions, but it can be easily installed using
  the ["getnonfreefonts" script](https://www.tug.org/fonts/getnonfreefonts/).
  A short guide on using this to get luximono can be found on the 
  TeX Stackexchange [here](http://tex.stackexchange.com/questions/22157/how-to-use-the-luximono-font-with-tex-live).

- The Heuristica font - this is an extension of the free version of the Adobe
  Utopia font. This must be installed as a system font for the PDF to
  build, and you can find the appropriate font package for your system
  here: <https://code.google.com/p/evristika/>


## General Advice for editors

- All files must be saved as UTF-8: ensure your editors are configured
  appropriately.

- Leave two empty lines between each section, regardless of level of nesting.
  Leave two empty lines at the end of every markdown file that forms a part
  of the main specification when compiled.

- Use of the appropriate unicode characters instead of the latex modifiers
  for accents, etc. is necessary. For example, é instead of \'e. Make use of
  the fact that the content is unicode, google the necessary characters if
  you don't know how to type them directly.


## Useful tools

I have found the following tools to be useful for viewing and testing the
output of the various builds:

- The [Markdown Preview](https://chrome.google.com/webstore/detail/markdown-preview/jmchmkecamhbiokiopfpnfgbidieafmd) extension for Chrome, for viewing
markdown files locally in the way GitHub renders it 
(you must enable viewing of local file urls for the extension). This is
useful for previewing markdown changes before comitting them, though it
will not render many of the Pandoc specific extensions.
- The [Readium](https://chrome.google.com/webstore/detail/empty-title/fepbnnnkkadjhjahcafoaglimekefifl) app for Chrome, for viewing epub3
files. This actually uses MathJAX to render the math elements on demand
for each page of the ebook.
- The [UnicodeMath](https://github.com/mvoidex/UnicodeMath) plugin for Sublime 
Text. This is helpful if you are familiar with LaTeX macros for mathematical 
symbols, as you may type the macro and have it automatically converted to the 
equivalent unicode symbol (if it exists). This can be installed through
the [Package Control](http://wbond.net/sublime_packages/package_control) 
Sublime Text plugin.


## Known issues and outstanding tasks

Please see the issue tracker at <https://github.com/iainmcgin/scala-ref-markdown/issues>.


## Fixing known build errors / warnings

### I am seeing `process\_defaultadd: n=-1 tag.data='NGENRE' level=0` every time I build the spec

Apparently this is due to a bug in the bibutils C library which was fixed,
and occurs if you have built pandoc from source. You must force a recompilation 
of a few Haskell libraries for the fixed bibutils library to be used. Simply 
run:

    ```
    cabal update
    cabal install --reinstall --force hs-bibutils citeproc-hs pandoc
    ```


## Fixing rendering errors

MathJAX errors will appear within the rendered DOM as span elements with
class `mtext` and style attribute `color: red` applied. It is possible to
search for this combination in the development tools of the browser of your
choice. In chrome, CTRL+F / CMD+F within the inspect element panel allows you
to do this.


## Conversion from LaTeX - Guidelines


### Chapter conversion Checklist

1. Convert all `\section{...}`
1. Convert all `\subsection{...}`
1. Convert all `\subsubsection{...}`
1. Convert all `{\em ...}`
1. Convert all `\lstlisting`
1. Convert all `\lstinline`
1. Convert all `\code`
1. Convert all `\sref{sec:...}`
1. Convert all `\begin{itemize}`
1. Convert all `\begin{enumerate}`
1. Convert all `\example`
1. Convert all `\footnote`
1. Convert all `\paragraph`
1. Convert all `\begin{quote}`
1. Delete all `\comment{...}`
1. Convert all single quote pairs
1. Convert all double quote pairs
1. Look for manually defined enumerated lists (1. 2. 3. etc)
1. Remove `%@M` comments
1. Convert all extra macros (`\commadots`, etc)


### Code

Code blocks using the listings package of form

    \begin{lstlisting}
    val x = 1
    val y = x + 1
    x + y
    \end{lstlisting}


can be replaced with pandoc code blocks of form

    ```{#ref-identifier .scala .numberLines}
    val x = 1
    val y = x + 1
    x + y
    ```

Where `#ref-identifier` is an identifier that can be used for producing links
to the code block, while `.scala` and `.numberLines` are classes that get 
applied to the code block for formatting purposes. At present we propose to
use the following classes:

- `.scala` for scala code.
- `.grammar` for EBNF grammars.

It is important to note that while math mode is supported in pandoc markdown
using the usual LaTeX convention, i.e. $x^y + z$, this does not work within 
code blocks. In most cases the usages of math mode I have seen within
code blocks are easily replaced with unicode character equivalents. If
a more complex solution is required this will be investigated at a later stage.


#### Inline Code

Inline code, usually `~\lstinline@...some code...@` can be replaced with
the pandoc equivalent of

    `...some code...`{<type>}

where `<type>` is one of the classes representing the language of the
code fragment.


### Definitions

Pandoc supports definition lists, however these do not seem to be a good
semantic match for the numbered definitions in the reference. The only
reasonable compromise I found was to treat definitions like quotations:

    > **Definition**
    > Let $C$ be a class with template ...


### Macro replacements:

- While MathJAX just support LaTeX style command definition, it is recommended
  to not use this as it will likely cause issues with preparing the document
  for PDF or ebook distribution.
- `\SS` (which I could not find defined within the latex source) seems to be
  closest to `\mathscr{S}`
- `\TYPE` is equivalent to `\boldsymbol{type}'
- As MathJAX has no support for slanted font (latex command \sl), so in all
  instances this should be replaced with \mathit{}
- The macro \U{ABCD} used for unicode character references can be
  replaced with \\uABCD.
- The macro \URange{ABCD}{DCBA} used for unicode character ranges can be
  replaced with \\uABCD-\\uDBCA.
- The macro \commadots can be replaced with ` , … , `.
- There is no adequate replacement for `\textsc{...}` (small caps) in pandoc 
  markdown. While unicode contains a number of small capital letters, it is
  notably missing Q and X as these glyphs are intended for phonetic spelling,
  therefore these cannot be reliably used. For now, the best option is to
  use underscore emphasis and capitalise the text manually, `_LIKE THIS_`.
- `\code{...}` can be replaced with standard in-line verbatim markdown,
  `` `like this` ``.
- `\paragraph` (typically used for a non-numbered header) can be replaced by 
  a hard line break, which is a `\` followed immediately by a newline.
- `\TODO` can be replaced by a markdown comment `<!-- TODO: ... -->`


### Unicode Character replacements

- The unicode left and right single quotation marks (‘ and ’) 
  have been used in place of ` and ', where the quotation marks are intended
  to be paired. These can be typed on a mac using Option+] for a left quote
  and Option+Shift+] for the right quote.
- Similarly for left and right double quotation marks (“ and ”) in
  place of ". These can be typed on a mac using Option+[ and Option+Shift+].


### Enumerations

Latex enumerations can be replaced with markdown ordered lists, which have
syntax

 1. first entry
 1. ...
 1. last entry

