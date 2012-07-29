
class A
class B extends A

class C {
  def foo(x: => Int)(y: String) = x
  def foo(x: String)(y: List[_]) = x
  def foo(x: => A)(y: Array[_]) = 1
  def foo(x: A)(y: Seq[_]) = 2
  def foo(x: B)(y: Map[_, _]) = 4
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val c = new C
  val im = cm.reflect(c)
  val t = u.typeOf[C] member u.newTermName("foo") asTermSymbol
  val f1 = t.resolveOverloaded(posVargs = List(u.typeOf[Int])) asMethodSymbol
  val f2 = t.resolveOverloaded(posVargs = List(u.typeOf[String])) asMethodSymbol
  val f3 = t.resolveOverloaded(posVargs = List(u.typeOf[A])) asMethodSymbol
  val f4 = t.resolveOverloaded(posVargs = List(u.typeOf[B])) asMethodSymbol
  val m1 = im.reflectMethod(f1)
  val m2 = im.reflectMethod(f2)
  val m3 = im.reflectMethod(f3)
  val m4 = im.reflectMethod(f4)
  assert(m1(() => 1, null) == c.foo(1)(null))
  assert(m2("a", null) == c.foo("a")(null))
  assert(m3(new A, null) == c.foo(new A)(null))
  assert(m4(new B, null) == c.foo(new B)(null))
}

