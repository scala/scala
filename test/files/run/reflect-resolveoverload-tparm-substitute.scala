
class A
class B extends A

class C {
  def foo[T](x: T) = x
  def foo(x: Int) = "a"
  def foo(x: A) = x
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val c = new C
  val im = cm.reflect(c)
  val term = u.typeOf[C] member u.newTermName("foo") asTermSymbol

  val f1 = term.resolveOverloaded(
    posVargs = List(u.typeOf[Int]),
    expected = u.typeOf[String]
  )

  val f2 = term.resolveOverloaded(
    targs = List(u.typeOf[String]),
    posVargs = List(u.typeOf[String]),
    expected = u.typeOf[String]
  )

  val f3 = term.resolveOverloaded(
    posVargs = List(u.typeOf[A]),
    expected = u.typeOf[A]
  )

  val f4 = term.resolveOverloaded(
    targs = List(u.typeOf[A]),
    posVargs = List(u.typeOf[A]),
    expected = u.typeOf[A]
  )

  val f5 = term.resolveOverloaded(
    targs = List(u.typeOf[B]),
    posVargs = List(u.typeOf[B]),
    expected = u.typeOf[B]
  )

  val f6 = term.resolveOverloaded(
    targs = List(u.typeOf[B]),
    posVargs = List(u.typeOf[B]),
    expected = u.typeOf[A]
  )

  val f7 = term.resolveOverloaded(
    targs = List(u.typeOf[A]),
    posVargs = List(u.typeOf[B]),
    expected = u.typeOf[A]
  )
  
  val m1 = im.reflectMethod(f1 asMethodSymbol)
  val m2 = im.reflectMethod(f2 asMethodSymbol)
  val m3 = im.reflectMethod(f3 asMethodSymbol)
  val m4 = im.reflectMethod(f4 asMethodSymbol)
  val m5 = im.reflectMethod(f5 asMethodSymbol)
  val m6 = im.reflectMethod(f6 asMethodSymbol)
  val m7 = im.reflectMethod(f7 asMethodSymbol)

  val a = new A
  val b = new B
  assert(m1(2) == (c.foo(2): String))
  assert(m2("xyz") == (c.foo[String]("xyz"): String))
  assert(m3(a) == (c.foo(a): A))
  assert(m4(a) == (c.foo[A](a): A))
  assert(m5(b) == (c.foo[B](b): B))
  assert(m6(b) == (c.foo[B](b): A))
  assert(m7(b) == (c.foo[A](b): A))


}
