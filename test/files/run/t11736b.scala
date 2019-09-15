// scalac: -Xelide-below ASSERTION

import scala.tools.partest.RunTest

trait Foo {
  trait Bar {
    assert(Foo.this ne null)        // succeeds when trait Foo1
  }
}

trait Foo1 extends Foo

object Foo2 extends Foo1 {
  new Bar {}
}

object Test extends RunTest {
  def run() = run(assertsOn)(Foo2: Unit)
}
