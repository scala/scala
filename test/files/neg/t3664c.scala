//> using options -Werror -Xlint -Xsource:3 -Xsource-features:case-companion-function

// use -Xsource:3 to warn that implicitly extending Function is deprecated
// use -Xsource-features for dotty behavior: no extend Function, yes adapt C.apply.tupled

case class C(i: Int, j: Int)

abstract case class D(i: Int, j: Int)

case class E()

case class F(i: Int)
object F {
  def apply(): F = apply(42)
  def apply(i: Int): F = new F(i)
  def apply(i: Int, j: Int): F = new F(i+j)
}

class Test {
  def f(xs: List[(Int, Int, Int)]): List[C] = xs.map(C) // hard error

  def g(xs: List[(Int, Int, Int)]): List[C] = xs.map(C.tupled) // hard error

  def d(xs: List[(Int, Int)]): List[D] = xs.map(D) // hard error

  val e: () => E = E // apply insertion warning, plus lint warning about 0-arity eta expansion

  def ov(xs: List[Int]): List[F] = xs.map(F)
}
