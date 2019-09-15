// scalac: -Xelide-below ASSERTION

import scala.tools.partest.BadRunTest

trait Foo {
  trait Bar {
    assert(Foo.this ne null)        // fails non-trivially
  }
}

abstract class Foo1 extends Foo

object Foo2 extends Foo1 {
  new Bar {}
}

object Test extends BadRunTest {
  def run() = run(assertsOn)(Foo2: Unit)
}
