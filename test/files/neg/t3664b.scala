//> using options -Werror -Xsource:3 -Xsource-features:case-companion-function -deprecation

// use -Xsource:3 to warn that implicitly extending Function is deprecated
// use -Xsource-features for dotty behavior: no extend Function, yes adapt C.apply.tupled

case class C(i: Int)
object O { case class D(i: Int) }

class Test {
  def f(xs: List[Int]): List[C] = xs.map(C) // ident
  def g(xs: List[Int]): List[O.D] = xs.map(O.D) // select
}
