
//> using options -Yrangepos

trait AA
trait BB
trait Foo {
  def consume(a: AA): Unit
}

object FooOpss {
  implicit class FooOps(val self: Foo) {
    def consume(a: BB): Unit = ???
  }
}
import FooOpss._

class Test {
  val theFoo: Foo = ???
  def doIt(id: Long): Unit =
    theFoo.consume(BBFactory.create(id))
}

object BBFactory {
  def create(id: Long)(implicit i: DummyImplicit): BB = ???
}

