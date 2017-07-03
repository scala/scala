package test

// this leading import into scala.Predef causes the default predef
// import scala.Predef._ to be excluded for this compilation unit
// resulting in a cleaner .check file
import scala.Predef.DummyImplicit

object O {
  def map(x: Int => Int)(implicit a: DummyImplicit): O.type = this
  val prefix123 : Int = 0
}

class Foo {
  O.map(x => x)./*!*/ // we want the presentation compiler to apply the implicit argument list.
}
