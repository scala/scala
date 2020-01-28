
// error message won't follow apply into object f,
// so it won't keep narrow type in z
//
class X { def f(n: Int) = 42 ; object f { def apply(i: Int) = 42 + i } }
class Y { def f(n: 42) = 42 ; def f[A <: 42](a: A) = 27 }
class Z { def f[A <: 42](a: A) = 42 ; object f { def apply(i: 42) = i } }

object Test extends App {
  val x = new X()
  def t1 = x.f(42)
  val n: 42 = 42
  def t2 = x.f(n)
  val y = new Y()
  def t3 = y.f(n)
  val z = new Z()
  println(z.f(n))
}
