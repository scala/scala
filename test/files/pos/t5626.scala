//> using options -feature -Wconf:cat=feature-reflective-calls:s -Werror
class C {
  val blob = {
    new { case class Foo() }
  }
  val blub = {
    class Inner { case class Foo() }
    new Inner
  }
  val z: Any { def x: X.type } = new { def x = X }

  val foo = blob.Foo()
  val bar = blub.Foo()
  val baz = z.x()
}

case class X()
