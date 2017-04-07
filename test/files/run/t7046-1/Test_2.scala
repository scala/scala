object Test extends App {
  val subs = Macros.knownDirectSubclasses[Foo]
  assert(subs == List("Wibble", "Wobble", "Bar", "Baz"))
}

sealed trait Foo
object Foo {
  trait Wibble extends Foo
  case object Wobble extends Foo
}

trait Bar extends Foo

object Blah {
  type Quux = Foo
}

import Blah._

trait Baz extends Quux

class Boz[T](t: T)
class Unrelated extends Boz(Test.subs)
