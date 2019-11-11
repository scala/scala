# TASTy Test

TASTy Test is a testing framework for scala 2 code that depends on code compiled with the `dotc`, the Scala 3 compiler, which outputs TASTy trees. The framework supports `run` and `neg` suites.

## `run`

A `run` suite tests the runtime behaviour of Scala 2 code that may extend or call into code compiled with `dotc`, and is specified as follows:
  1) Compile sources in `run/pre/**/` with the Scala 2 compiler, this step may be used to create helper methods or embedded test runners.
  2) Compile sources in `run/src-3/**/` with the Dotty compiler. Classes compiled in `(1)` are now on the classpath.
  3) Compile sources in `run/src-2/**/` with the Scala 2 compiler. Classes compiled in `(1)` and `(2)` are now on the classpath.
  4) All compiled classes are filtered for those with file names that match the regex `(.*Test)\.class`, and have a corresponding source file in `run/src-2/**/` matching `$1.scala`, where `$1` is the substituted name of the class file. The remaining classes are executed sequentially as tests:
     - A test class must have a static method named `main` and with descriptor `([Ljava.lang.String;)V`.
     - The `out` and `err` print streams of `scala.Console` are intercepted before executing the `main` method.
     - A successful test should print the single line `Suite passed!` to the `Console` and not throw any runtime exceptions that escape `main`.
## `neg`
A `neg` suite asserts which Scala 2 code is not compatible with code compiled with `dotc`, and is specified as follows:
  1) Compile sources in `neg/src-3/**/` with the Dotty compiler.
  2) Source files in `neg/src-2/**/` are filtered for those with names that match the regex `(.*)_fail.scala`, and an optional check file that matches `$1.check` where `$1` is the substituted test name, and the check file is in the same directory. These are sources expected to fail compilation.
  3) Compile scala sources in `neg/src-2/**/` with the Scala 2 compiler.
     - Classes compiled in `(1)` are now on the classpath.
     - If a Scala source fails compilation, check that it is in the set of expected fail cases, and that there is a corresponding check file that matches the compiler output, else collect in the list of failures.
     - If an expected fail case compiles successfully, collect it in the list of failures.

## General Notes
- In each suite, the dotty library is available to all test sources.
- In either suite's source directory, failing tests without a known fix should be put in a sibling directory to `src-2`, `src-3`, etc., such as `suspended`, to document that they are incompatible at present.
