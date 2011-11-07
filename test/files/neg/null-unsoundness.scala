class B
class C(x: String) extends B

class A {
  type A >: Null
  class D { type T >: C <: B }
  val x: A with D = null
  var y: x.T = new C("abc")
}
object Test extends A with App {
  class C { type T = Int; val x = 1 }
  type A = C
  y = 42
}
  
