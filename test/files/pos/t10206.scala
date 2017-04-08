class Foo(val bar: String)

object Foo {
  implicit class Enrich(foo: Foo) {
    def clone(x: Int, y: Int): Int = x + y
  }
}

object Main extends App {
  val foo = new Foo("hello")
  println(foo.clone(1, 2))    // <- does not compile
  // the implicit view was being disqualified because a new check in the compiler
  // that implicit views must not target Any or AnyRef considered an implicit search
  // for `foo.type => ?{def clone: ?}` to targeted AnyRef.
}
