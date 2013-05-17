sealed abstract class Foo {
  @inline def bar(x: Int) = x + 1
}
object Foo {
  def mkFoo(): Foo = new Baz2
}

object Baz1 extends Foo
final class Baz2 extends Foo

object Test {
  // bar should be inlined now
  def f = Foo.mkFoo() bar 10
}
