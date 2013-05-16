class B
class C(x: String) extends B

abstract class A {
  class D { type T >: C <: B }
  val x: D
  var y: x.T = new C("abc")
}

class Volatile extends A {
  type A >: Null
  // test (1.4), pt 2 in RefChecks
  val x: A with D = null
}
