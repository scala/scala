package tastytest

object TraitParams:
  trait Foo(a: String)
  trait Bar(x: Int, y: String)
  trait Baz(val baz: String)
  trait Qux(val qux: String)
  val foo: Foo = ???
