// scalac: -Xlint -Xfatal-warnings

class C {
  def f = 1
  def g(x: Int) = 0
  def h(x: Int)(y: Int) = 0
  def i(x: => Int)(y: Int) = 0
}
object Test {
  def t1 = (new C).g _           // warn
  def t2: Int => Int = (new C).g // warn
  def t3(c: C) = c.g _           // ok
  def t4(c: C): Int => Int = c.g // ok
  def t5(c: C) = c.h(1) _        // ok
  def t6(c: C) = c.h(c.f) _      // warn
  def t7(c: C) = c.i(c.f) _      // ok (by-name)
}
