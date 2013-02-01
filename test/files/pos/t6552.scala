object Repros {
  class Bar {}
  class Baz(val myFoo: Foo) { }
  trait Foo {
    this: Bar =>
    val thing = new Baz(this)
  }
}
