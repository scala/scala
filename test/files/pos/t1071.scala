class C {
  private val a = 0
  def getA = a
}

class D(c: C) {
  def a = c.getA
}

object Test {
  implicit def c2d(c: C): D = new D(c)

  val c = new C
  (c: D).a // works
  c.a // error
}

