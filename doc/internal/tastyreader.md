# TASTy Reader For Scala 2

The [**TASTy Reader For Scala 2**](https://scala.epfl.ch/projects.html#tastyScala2), included in the Scala 2.x Compiler will enable dependencies to be used which are compiled with dotc, the reference compiler of Scala 3.

## Testing

The framework for testing TASTy reader is contained in the `tastytest` subproject. A description of its functionality is provided at [tastytest.md](tastytest.md)

The `tasty` project is an example subproject depending on `tastytest`, used to test the functionality of the TASTy reader. Test sources are placed in the `test/tasty` directory of this repository and tested with the sbt task `tasty/test`.

Individual tasks exist to help debug issues:

- `tasty/dotc <out directory> <filename>` to compile a Scala 3 source file with the supported version of dotty, where the classpath is set to the out directory.
- `tasty/dotcd <filename> <arg>*` to decompile a tasty file with the supported version of dotty. Can pass additional flags.
- `tasty/scalac <out directory> <filename> <arg>*` to compile a Scala 2 file, where the classpath is set to the out directory, along with the dotty library. Can pass additional flags.
- `tasty/runDotty <classpath> <classname>` to execute a main method with the dotty library added to the given classpath.

## Notes

comments beginning with `// TODO [tasty]:` should be considered carefully as they are special accomodations for the TASTy reader that are particulary expensive, or leak out of the classfile parser or `scala.tools.nsc.tasty` package.
