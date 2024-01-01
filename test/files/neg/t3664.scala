
//> using options -Werror -Xlint -Xsource:3
//> abusing options -Werror -Xlint -Xsource:3migration

// use 3migration to warn that implicitly extending Function is deprecated
// use 3cross for dotty behavior: no extend Function, yes adapt C.apply.tupled

case class C(i: Int)

class Test {
  def f(xs: List[Int]): List[C] = xs.map(C)
}
