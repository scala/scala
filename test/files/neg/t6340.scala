object Foo {
  class A
  class B
  class C
  class X
  class Y
  class Z
}

object Test {
  import Foo.{ A, B, C, D, E, X, Y, Z }

  val a = new A
  val b = new B
  val c = new C
  val d = new D
  val w = new W
  val x = new X
  val y = new Y
  val z = new Z
}
