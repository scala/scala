
//> using options -opt:inline:<sources> -Wopt -Werror
// skalac: -opt:inline:<sources> -Vopt:C -Wopt -Werror

// > using scala 2.13.nightly
// > using options -opt:inline:<sources>, -Wopt

import scala.collection.LinearSeq

final class C {
  val iterators: LinearSeq[Int] = List(42)
  @inline def current: Int = iterators.head
  val asString = current.toString
}
object Test extends App {
  val c = new C
  println(c.asString)
}
