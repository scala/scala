package tastytest

import scala.language.experimental.macros

object TestStaticAnnotations extends Suite("TestStaticAnnotations") {

  def hasStrictFP[T]: Boolean = macro Macros.hasStaticAnnotImpl[T, scala.annotation.strictfp]

  class Foo extends Floating

  test("Foo.add has strictfp")(assert(hasStrictFP[Foo]))

}
