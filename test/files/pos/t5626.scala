class C {
  val blob = {
    new { case class Foo() }
  }
  val blub = {
    class Inner { case class Foo() }
    new Inner
  }

  val foo = blob.Foo()
  val bar = blub.Foo()
}
