package test

import scala.Predef.DummyImplicit // turn off other predef implicits for a cleaner .check file.

object O {
  def map(x: Int => Int)(implicit a: DummyImplicit): O.type = this
  val prefix123 : Int = 0
}

class Foo {
  O.map(x => x)./*!*/ // we want the presentation compiler to apply the implicit argument list.
}
