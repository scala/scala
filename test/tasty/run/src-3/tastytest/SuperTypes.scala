package tastytest

object SuperTypes {

  class Foo {
    final val foo: "Foo.foo" = "Foo.foo"
  }

  class Bar extends Foo {

    object A {
      def unapply(a: Any) = Some[Bar.super.foo.type](Bar.this.foo)
    }

    def bar: Bar.super.foo.type = "" match { case A(x) => x }
  }

  class Baz extends Foo {

    object A {
      def unapply(a: Any) = Some[Baz.super[Foo].foo.type](Baz.this.foo)
    }

    def baz: Baz.super[Foo].foo.type = "" match { case A(x) => x }
  }
}
