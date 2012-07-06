
class A {
  override def equals(x: Any) = {
    x.isInstanceOf[A] && !x.isInstanceOf[B]
  }
}
class B extends A {
  override def equals(x: Any) = {
    x.isInstanceOf[B]
  }
}

class C {
   def a(x: String) = 1
   def a(x: Array[_]) = "a"
   def b(x: String) = new A
   def b(x: Array[_]) = new B
   def c(x: String) = new B
   def c(x: Array[_]) = "a"
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val c = new C
  val im = cm.reflect(c)
  def invoke(s: String, expectedType: u.Type, expectedResult: Any) {
    val ol = (u.typeOf[C] member u.newTermName(s)).asTermSymbol
    val methodSym = ol.resolveOverloaded(posVargs = List(u.typeOf[Null]), expected = expectedType).asMethodSymbol
    val sig = methodSym.typeSignature.asInstanceOf[u.MethodType]
    val method = im.reflectMethod(methodSym)
    assert(method(null) == expectedResult)
  }

  invoke("a", u.typeOf[Int], c.a(null): Int)
  invoke("a", u.typeOf[String], c.a(null): String)
  invoke("b", u.typeOf[B], c.b(null): B)
  invoke("c", u.typeOf[A], c.c(null): A)
  invoke("c", u.typeOf[A], c.c(null): A)
  invoke("c", u.typeOf[B], c.c(null): B)
  invoke("c", u.typeOf[String], c.c(null): String)

}
