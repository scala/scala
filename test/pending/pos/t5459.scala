trait A1
trait A2
trait A3
trait L1 extends A1 with A2 with A3

object Test {
  trait T1[-A <: A1]
  trait T2[-A >: L1]
  trait T3[ A <: A1]
  trait T4[ A >: L1]
  trait T5[+A <: A1]
  trait T6[+A >: L1]

  def f1(x: T1[_]) = x
  def f2(x: T2[_]) = x
  def f3(x: T3[_]) = x
  def f4(x: T4[_]) = x
  def f5(x: T5[_]) = x
  def f6(x: T6[_]) = x
  // a.scala:22: error: type arguments [Any] do not conform to trait T5's type parameter bounds [+A <: A1]
  //   def f5(x: T5[_]) = x
  //       ^

  def g1(x: T1[_ <: A1]) = x
  def g2(x: T2[_ >: L1]) = x
  def g3(x: T3[_ <: A1]) = x
  def g4(x: T4[_ >: L1]) = x
  def g5(x: T5[_ <: A1]) = x
  def g6(x: T6[_ >: L1]) = x

  def q1(x: T1[_ >: L1]) = x
  def q2(x: T2[_ <: A1]) = x
  def q3(x: T3[_ >: L1]) = x
  def q4(x: T4[_ <: A1]) = x
  def q5(x: T5[_ >: L1]) = x
  def q6(x: T6[_ <: A1]) = x
  // a.scala:41: error: type arguments [Any] do not conform to trait T5's type parameter bounds [+A <: A1]
  //   def q5(x: T5[_ >: L1]) = x
  //       ^
  // two errors found

  def h1(x: T1[_ >: L1 <: A1]) = x
  def h2(x: T2[_ >: L1 <: A1]) = x
  def h3(x: T3[_ >: L1 <: A1]) = x
  def h4(x: T4[_ >: L1 <: A1]) = x
  def h5(x: T5[_ >: L1 <: A1]) = x
  def h6(x: T6[_ >: L1 <: A1]) = x
}
