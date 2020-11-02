# TASTy Reader For Scala 2

The [**TASTy Reader For Scala 2**](https://scala.epfl.ch/projects.html#tastyScala2), included in the Scala 2.x Compiler will enable usage in Scala `2.13.x` of dependencies that have been compiled with `dotc`, the reference compiler of Scala `3.0`.

TASTy is an intermediate representation of a Scala program after type checking and term elaboration, such as inference of implicit parameters. When compiling code with Scala 3, a single TASTy document is associated with each pair of root class and companion object. Within a TASTy document, the public API of those roots and any inner classes can be read, in a similar way to pickles in the Scala 2.x series.

## Working with the code

### Compiler flags

- `-Ytasty-reader` enables the support for reading Scala 3's TASTy files.

- `-Ydebug-tasty` enables rich output when traversing tasty files, important for tracing the history of events when diagnosing errors.

- `-Ytasty-no-annotations` ignores all annotations on tasty symbols, may be useful for ignoring complex annotations that are unsupported, but will prevent safety checks for pattern matching.

### Entry Points

A classfile is assumed to have an associated TASTy file if it has a `TASTY` classfile attribute (not available through
Java reflection). This attribute contains a UUID that matches a UUID in the header of a sibling `.tasty` file of the
same directory as the classfile. This file is then found and the UUIDs are compared in
`scala.tools.nsc.symtab.classfile.ClassfileParser`.
After validation of the header, the tasty file is traversed in `scala.tools.nsc.tasty.TastyUnpickler.unpickle`, which
reads any definitions into the symbol table of the compiler.

### Concepts in TASTy

A TASTy document is composed of a header, which contains a magic number `0x5CA1AB1F`, a version number and a UUID.
The TASTy document then is composed of a list of names, followed by customisable "sections". The section we are
interested in for Scala 2 is the "ASTs" section. The ASTs section contains a package definition for the root class and
companion of the tasty file. In TASTy, both terms and types are made of trees, and sometimes trees can be reused in
either term or type position, for example path selections. There are five main concepts in TASTy:
  - Name: has many roles
    - An identifier associated with a Symbol,
    - A cursor to lookup terms or types within the scope of a parent type, including resolving a specific overload,
      or distinguishing between a class and its companion object's implementation class.
    - To describe the erased signature of an method.
  - Flags: an enumerated set of properties for a Symbol, e.g. if it is a Method, Object, Param, etc.
  - Symbol: an aggregate of Flags, a Name and a Type, representing the semantic information about a definition
  - Type: corresponds to a scala reflect Type, can be lazy
  - Term: corresponds to a scala reflect Tree and has a Type. Annotations are represented as Terms

### Workflow

A typical workflow for experimenting with the TASTy reader is to:
1) create a workspace directory `$issue`, e.g. `sandbox/issue`
2) create an output directory `$out`, e.g. `$issue/bin`
3) create a Scala 3 source file `$src3`, e.g. `$issue/FancyColours.scala`
4) compile the Scala 3 source file to `$out`:
   - `dotc -d $out $src3`
5) create a Scala 2 source file, `$src2`, that uses some symbols from `$src3`, e.g. `$issue/TestFancyColours.scala`
6) compile the Scala 2 source file, adding any symbols from `$src3` to the classpath:
   - `scalac -Ydebug-tasty -d $out -classpath $out $src2`

Here are some example source files from the above scenario:
```scala
// FancyColours.scala - compile with Scala 3

trait Pretty:
  self: Colour =>

trait Dull:
  self: Colour =>

enum Colour:
  case Pink extends Colour with Pretty
  case Red  extends Colour with Dull
```
```scala
// TestFancyColours.scala - compile with Scala 2

object TestFancyColours {

  def describe(c: Colour) = c match {
    case Colour.Pink => "Amazing!"
    case Colour.Red  => "Yawn..."
  }

  def describePretty(c: Pretty) = c match {
    case Colour.Pink => "Amazing!"
  }

  def describeDull(c: Dull) = c match {
    case Colour.Red => "Yawn..."
  }

}
```

The [Script Runner](#script-runner) section describes some commands that support this workflow and can be run from sbt; which also handles providing the supported version of `dotc` on the classpath.

Below is an example of using the [Script Runner](#script-runner) to simplify iterative development of the scenario above:

1) First, compile the Scala 3 code with `tasty/test:runMain scala.tools.tastytest.Scripted dotc $out $issue/FancyColours.scala`.
2) Next, compile the test code from Scala 2 with `tasty/test:runMain scala.tools.tastytest.Scripted scalac $out $issue/TestFancyColours.scala -Ydebug-tasty`, which will also put the contents of `$out` on the classpath.
3) To aid with debugging, inspect the TASTy structure for `Colour` with `tasty/test:runMain scala.tools.tastytest.Scripted dotcd $out/Colour.tasty -print-tasty`

In the above, relative paths will be calculated from the working directory of `tasty/test`.

Because these commands are run from sbt, incremental changes can be made to the code for the TASTy reader and then step `2` can be immediately re-run to observe new behaviour of the compiler.

In the output of the above step `2`, you will see the the following snippet, showing progress in traversing TASTy and understanding the definition of `trait Dull`:
```scala
#[trait Dull]: Addr(4) completing Symbol(trait Dull, #6286):
#[trait Dull]: Addr(7) No symbol found at current address, ensuring one exists:
#[trait Dull]: Addr(7) registered Symbol(value <local Dull>, #7240) in trait Dull
#[trait Dull]: Addr(9) Template: reading parameters of trait Dull:
#[trait Dull]: Addr(9) Template: indexing members of trait Dull:
#[trait Dull]: Addr(22) No symbol found at current address, ensuring one exists:
#[trait Dull]: Addr(22) ::: => create DEFDEF <init>
#[trait Dull]: Addr(22) parsed flags Stable | Method
#[trait Dull]: Addr(22) registered Symbol(constructor Dull, #7241) in trait Dull
#[trait Dull]: Addr(9) Template: adding parents of trait Dull:
#[trait Dull]: Addr(9) reading type TYPEREF:
#[trait Dull]: Addr(11) reading type TERMREFpkg:
#[trait Dull]: Addr(13) Template: adding self-type of trait Dull:
#[trait Dull]: Addr(15) reading term IDENTtpt:
#[trait Dull]: Addr(17) reading type TYPEREF:
#[trait Dull]: Addr(19) reading type THIS:
#[trait Dull]: Addr(20) reading type TYPEREFpkg:
#[trait Dull]: Addr(22) Template: self-type is Colour
#[trait Dull]: Addr(22) Template: Updated info of trait Dull extends AnyRef
#[trait Dull]: Addr(4) typeOf(Symbol(trait Dull, #6286)) =:= Dull; owned by package <empty>
```

### Tagged comments
Comments beginning with `TODO [tasty]:` express concerns specific to the implementation of the TASTy reader. These should be considered carefully because of either the disruptive changes they make to the rest of the code base, or as a note that there may be a more correct solution, or as a placeholder to outline missing features of Scala 3 that are not yet backported to Scala 2.x.

## Testing

The framework for testing the TASTy reader is contained in the `tastytest` subproject.

The `tasty` project is an example subproject depending on `tastytest`, used to test the functionality of the TASTy
reader. Test sources are placed in the `test/tasty` directory of this repository and tested with the sbt task
`tasty/test`. Several suites exist that build upon primitives in `tastytest`:
- `run`: test that classes can depend on Scala 3 classes and execute without runtime errors.
- `neg`: assert that scala 2 test sources depending on Scala 3 classes do not compile
- `neg-isolated`: assert that code depending on symbols not on the classpath fails correctly.
- `pos`: The same as `run` except with no runtime checking, useful for validating types while waiting for bytecode to align.
- `pos-false-noannotations`: the same as `pos` but asserting code falsely compiles without warnings or errors when annotations are ignored.

### Script Runner

A key tool for working with the tasty reader on individual test cases is `scala.tools.tastytest.Scripted`. It provides several sub commands which share a common implementation with the core of `tastytest`, meaning that the behaviour is identical.
Each sub command is executed with the Dotty standard library and tooling on the classpath, with the version determined by
`TastySupport.dottyCompiler` in the build definition. All relative paths will use the working directory `tasty/test`:

In the sbt shell the `scripted` runner can be executed by `tasty/test:runMain scala.tools.tastytest.Scripted`, and provides several sub-commands:
- `dotc <out: Directory> <src: File>`: compile a Scala 3 source file, which may depend on classes already compiled in `out`.
- `dotcd <tasty: File> <args: String*>`: decompile a tasty file, pass `-print-tasty` to see the structure of the ASTs.
- `scalac <out: Directory> <src: File> <args: String*>`: compile a Scala 2 source file, which may depend on classes already compiled in `out`, including those compiled by Scala 3. `args` can be used to pass additional scalac flags, such as `-Ydebug-tasty`
- `runDotty <classpath: Paths> <classname: String>`: execute the static main method of the given class, and providing no arguments.
- `javac <out: Directory> <src: File>`: compile a Java source file matching, which may depend on classes already compiled in `out`.

### tastytest Runner

`tastytest` is a testing library for validating that Scala 2 code can correctly depend on classes compiled with `dotc`, the Scala 3 compiler, which outputs TASTy trees. The framework has several suites for testing different scenarios. In each suite kind, the Scala 3 standard library is available to all test sources. `tastytest` does not implement the TestInterface so it is recommended to call its entry points from JUnit, like in `test/tasty/test/scala/tools/tastytest/TastyTestJUnit.scala`.

#### run Suites
A `run` suite tests the runtime behaviour of Scala 2 code that may extend or call into code compiled with `dotc`, and is specified as follows:

  1) A root source `$src` is declared, e.g. `"run"`
  2) Compile sources in `$src/pre/**/` with the Scala 2 compiler, this step may be used to create helper methods available to Scala 2 and 3 sources, or embedded test runners.
  3) Compile sources in `$src/src-3/**/` with the Dotty compiler. Classes compiled in `(2)` are now on the classpath.
  4) Compile sources in `$src/src-2/**/` with the Scala 2 compiler. Classes compiled in `(2)` and `(3)` are now on the classpath.
  5) All compiled classes are filtered for those with file names that match the regex `(.*Test)\.class`, and have a corresponding source file in `$src/src-2/**/` matching `$1.scala`, where `$1` is the substituted name of the class file. The remaining classes are executed sequentially as tests:
     - A test class must have a static method named `main` and with descriptor `([Ljava.lang.String;)V`.
     - The `out` and `err` print streams of `scala.Console` are intercepted before executing the `main` method.
     - A successful test must not output to either stream and not throw any runtime exceptions that are not caught within `main`.

#### pos Suites
A `pos` suite tests the compilation of Scala 2 code that may extend or call into code compiled with `dotc`, and is specified the same as `run`, except that step `(5)` is skipped. If step `(4)` succeeds then the suite succeeds.

#### neg Suites
A `neg` suite asserts which Scala 2 code is not compatible with code compiled with `dotc`, and is specified as follows:
  1) A root source `$src` is declared, e.g. `"neg"`
  2) Compile sources in `$src/src-3/**/` with the Dotty compiler.
  3) Source files in `$src/src-2/**/` are filtered for those with names that match the regex `(.*)_fail.scala`, and an optional check file that matches `$1.check` where `$1` is the substituted test name, and the check file is in the same directory. These are sources expected to fail compilation.
  4) Compile scala sources in `$src/src-2/**/` with the Scala 2 compiler.
     - Classes compiled in `(2)` are now on the classpath.
     - If a Scala source fails compilation, check that it is in the set of expected fail cases, and that there is a corresponding check file that matches the compiler output, else collect in the list of failures.
     - If an expected fail case compiles successfully, collect it in the list of failures.

#### neg-isolated Suites
A `neg-isolated` suite tests the effect of missing transitive dependencies on the classpath that are available to Scala 3 dependencies of Scala 2 sources, but not to those downstream Scala 2 sources, and is specified as follows:
  1) A root source `$src` is declared, e.g. `"neg-isolated"`
  2) Compile sources in `$src/src-3-A/**/` with the Dotty compiler.
  3) Compile sources in `$src/src-3-B/**/` with the Dotty compiler. Classes compiled in `(2)` are now on the classpath.
  3) Identical to `neg` step `(3)`
  4) Compile scala sources in `$src/src-2/**/` with the Scala 2 compiler. Test validation behaviour matches `neg` step `(4)`, except for the following caveats:
     - Only classes compiled in `(3)` will be on the classpath. Classes compiled in `(2)` are deliberately hidden.
     - References to symbols compiled in `(3)` that reference symbols in compiled in `(2)` should trigger missing symbol errors due to the missing transitive dependency.
