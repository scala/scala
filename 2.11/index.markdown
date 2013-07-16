# Overview of Scala 2.11
# Smaller
We're modularizing the standard library and the compiler to allow more selective use of Scala's features, and to facilitate contributions to more clearly delineated sub-projects.

The library opens up along the following fault lines: scala-library, scala-xml, scala-parser-combinators, ... (TODO).
The compiler platform will provide the following services: scala-compiler, scala-interactive, scala-scaladoc, and scala-repl.

We're also slimming down by removing dead or deprecated code, and enabling future weight loss through aggressive deprecation:
  - The old implementations of the Pattern Matcher and the Bytecode Emitter have been removed, as they were replaced wholesale in Scala 2.10. The experimental .NET backend had been scrapped, and the search and destroy mission in #1648 snuffed ~5000 chunks of dead code. 
  - The following packages have been deprecated:
    - `scala.actors`: see the [actors migration guide](http://docs.scala-lang.org/overviews/core/actors-migration-guide.html)
    - `scala.text`
    - TODO

## XML
The package scala.xml has been moved out of scala-library.jar.
To compile code that contains XML literals, add a dependency on scala-xml or your preferred alternative.

# Faster
Branch elimination through constant analysis #2214
Improve performance of reflection SI-6638

# Stronger

## Language
  - Case classes with > 22 parameters are now supported SI-7296
  - Infer bounds of existential types SI-1786

## REPL
  - @som-snytt added and improved several commands (:javap, :paste, :edit,...)
  - @folone and @eed3si9n contributed the `:kind` command to help to tell ground types from type constructors (#2340)
  - @rjolly made it possible to embed the repl as a JSR-166 Scripting Engine (#2206).


(Please contact @adriaanm before changing this page.)
