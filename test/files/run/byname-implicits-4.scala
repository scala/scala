trait Foo { def next: Foo }
object Foo {
  implicit def foo(implicit rec: => Foo): Foo = new Foo { def next = rec }
}

object Test extends App {
  val foo = implicitly[Foo]
  assert(foo eq foo.next)
}

