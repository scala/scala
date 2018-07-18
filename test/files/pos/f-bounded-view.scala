object Foo {
  implicit def toBar[T <: Bar[T]](t: T): Baz = ???
}

import Foo._

trait Bar[T]

class Baz {
  def wibble = 23
}

class Quux extends Bar[Quux] {
  def blah = this.wibble
}

object Test {
  (new Quux).blah
}
