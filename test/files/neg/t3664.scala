//> using options -Werror -Xlint -Xsource:3

// use -Xsource:3 to warn that implicitly extending Function is deprecated
// use -Xsource:3-cross for dotty behavior: no extend Function, yes adapt C.apply.tupled

case class C(i: Int)

class Test {
  def f(xs: List[Int]): List[C] = xs.map(C)
}
