package tastytest

object TestPathDep extends Suite("TestPathDep") {

  class FooString extends PathDep.Foo {
    type T = String
    def foo = "I am foo"
  }

  class FooInt extends PathDep.Foo {
    type T = Int
    def foo = 23
  }

  class GooInt extends PathDep.Goo {
    type F = FooInt
    val foo = new FooInt
  }

  class GooString extends PathDep.Goo {
    type F = FooString
    val foo = new FooString
  }

  val b = new PathDep.Bar

  test(assert((b.bar(new FooString): String) === "I am foo"))
  test(assert((b.bar(new FooInt): Int) === 23))

  test(assert((b.baz(new GooString): String) === "I am foo"))
  test(assert((b.baz(new GooInt): Int) === 23))

  test(assert((b.qux(new GooString).foo: String) === "I am foo"))
  test(assert((b.qux(new GooInt).foo: Int) === 23))

}
