class A
class B extends A

class C {
   def a(x: Int) = 1
   def a(x: String) = 2
   //def b(x: => Int)(s: String) = 1
   //def b(x: => String)(a: Array[_]) = 2
   def c(x: A) = 1
   def c(x: B) = 2
   //def d(x: => A)(s: String) = 1
   //def d(x: => B)(a: Array[_]) = 2
   def e(x: A) = 1
   def e(x: B = new B) = 2
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val c = new C
  val im = cm.reflect(c)
  def invoke(s: String, arg: Any, argType: u.Type): Int = {
    val ol = u.typeOf[C] member u.newTermName(s) asTermSymbol
    val methodSym = ol.resolveOverloaded(posVargs = List(argType)) asMethodSymbol
    val sig = methodSym.typeSignature.asInstanceOf[u.MethodType]
    val method = im.reflectMethod(methodSym)
    if (sig.resultType.kind == "MethodType") method(arg, null).asInstanceOf[Int]
    else method(arg).asInstanceOf[Int]
  }
  assert(c.a(1) == invoke("a", 1, u.typeOf[Int]))
  assert(c.a("a") == invoke("a", "a", u.typeOf[String]))
  //assert(c.b(1)(null) == invoke("b", 1, u.typeOf[Int]))
  //assert(c.b("a")(null) == invoke("b", "a", u.typeOf[String]))
  assert(c.c(new A) == invoke("c", new A, u.typeOf[A]))
  assert(c.c(new B) == invoke("c", new B, u.typeOf[B]))
  //assert(c.d(new A)(null) == invoke("d", new A, u.typeOf[A]))
  //assert(c.d(new B)(null) == invoke("d", new B, u.typeOf[B]))
  assert(c.e(new A) == invoke("e", new A, u.typeOf[A]))
  assert(c.e(new B) == invoke("e", new B, u.typeOf[B]))
}
