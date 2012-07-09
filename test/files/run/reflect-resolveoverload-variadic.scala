
class C {
  def foo(x: Int*) = 1 + x.sum
  def foo(x: String) = 2
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val c = new C
  val im = cm.reflect(c)
  val foo = u.typeOf[C] member u.newTermName("foo") asTermSymbol
  val f0 = foo.resolveOverloaded()
  val f1 = foo.resolveOverloaded(posVargs = Seq(u.typeOf[Int]))
  val f2 = foo.resolveOverloaded(posVargs = Seq(u.typeOf[Int], u.typeOf[Int]))
  val f3 = foo.resolveOverloaded(posVargs = Seq(u.typeOf[String]))

  val m0 = im.reflectMethod(f0 asMethodSymbol)
  val m1 = im.reflectMethod(f1 asMethodSymbol)
  val m2 = im.reflectMethod(f2 asMethodSymbol)
  val m3 = im.reflectMethod(f3 asMethodSymbol)
  
  assert(m0(Seq()) == c.foo())
  assert(m1(Seq(1)) == c.foo(1))
  assert(m2(Seq(4, 9)) == c.foo(4, 9))
  assert(m3("abc") == c.foo("abc"))
}
