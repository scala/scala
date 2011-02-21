object Test extends App {
  case class A(x: Int);

  class Foo(a: A) { println("Foo created!"); def +(x: Int) = new A(this.a.x + x); }

  implicit def aToFoo(x: A) = new Foo(x);

  println(A(3) + "H")

}
