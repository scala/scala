package tastytest

import scala.language.experimental.macros

object TestRuntimeAnnotations extends Suite("TestRuntimeAnnotations") {

  // def hasStrictFP[T](x: T): Boolean = macro Macros.hasStrictFPImpl[T]

  class Foo extends Floating

  // test("Foo.add has strictfp")(assert(hasStrictFP(new Foo()))) // suspended until annotations are supported.
}
