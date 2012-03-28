object Test {
  val blob0 = new {
    case class Foo(i : Int)
  }
  val foo0 = blob0.Foo(22)

  val blob1 = new {
    class Foo(i: Int)
    object Foo { def apply(i: Int): Foo = new Foo(i) }
  }
  val foo1 = blob1.Foo(22)
}
