
class A
class B extends A

class C {
  def a(x: Int) = 1
  def a(x: String) = 2
  def b(x: B) = 3
  def c(x: A, y: B) = 4
  def c(x: B, y: A) = 5
  def d[T](x: Int) = 6
  def d(x: String) = 7
  def e(x: A) = 8
  def e(x: =>B) = 9
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe

  val x = new C
  val t = u.typeOf[C]

  val a = t member u.newTermName("a") asTermSymbol
  val b = t member u.newTermName("b") asTermSymbol
  val c = t member u.newTermName("c") asTermSymbol
  val d = t member u.newTermName("d") asTermSymbol
  val e = t member u.newTermName("e") asTermSymbol

  val n1 = a.resolveOverloaded(posVargs = List(u.typeOf[Char]))
  val n2 = b.resolveOverloaded(posVargs = List(u.typeOf[A]))
  val n3 = c.resolveOverloaded(posVargs = List(u.typeOf[B], u.typeOf[B]))
  val n4 = d.resolveOverloaded(targs = List(u.typeOf[Int]))
  val n5 = d.resolveOverloaded()
  val n6 = e.resolveOverloaded(posVargs = List(u.typeOf[B]))
  
  assert(n1 == u.NoSymbol)
  assert(n2 == u.NoSymbol)
  assert(n3 == u.NoSymbol)
  assert(n4 == u.NoSymbol)
  assert(n5 == u.NoSymbol)
  assert(n6 == u.NoSymbol)
}
