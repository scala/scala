package neg1 {
  sealed abstract class Foo {
    @inline def bar(x: Int) = x + 1
  }
  object Foo {
    def mkFoo(): Foo = new Baz2
  }

  object Baz1 extends Foo
  final class Baz2 extends Foo
  final class Baz3 extends Foo {
    override def bar(x: Int) = x - 1
  }

  object Test {
    // bar can't be inlined - it is overridden in Baz3
    def f = Foo.mkFoo() bar 10
  }
}

package neg2 {
  sealed abstract class Foo {
    @inline def bar(x: Int) = x + 1
  }
  object Foo {
    def mkFoo(): Foo = new Baz2
  }

  object Baz1 extends Foo
  final class Baz2 extends Foo
  class Baz3 extends Foo {
    override def bar(x: Int) = x - 1
  }

  object Test {
    // bar can't be inlined - Baz3 is not final
    def f = Foo.mkFoo() bar 10
  }
}
