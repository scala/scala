//> using options -Xsource:3 -Xsource-features:case-companion-function,eta-expand-always -deprecation

// use -Xsource:3 to warn that implicitly extending Function is deprecated
// use -Xsource-features for dotty behavior: no extend Function, yes adapt C.apply.tupled

case class B(i: Int)
case class C(i: Int, j: Int)

class Test {
  def mapped(xs: List[Int]): List[B] = xs.map(B)

  // accept for cross because dotty has no C.tupled but has fancy untupling adaptation
  def cross(xs: List[(Int, Int)]): List[C] = xs.map(C)

  def f(xs: List[(Int, Int)]): List[C] = xs.map(C.tupled)

  def g(xs: List[Int]): List[C] = xs.map(C.curried).map(_(42))

  def f2(xs: List[(Int, Int)]): List[C] = xs.map((C.apply _).tupled)

  def g2(xs: List[Int]): List[C] = xs.map((C.apply _).curried).map(_(42))

  def g3(xs: List[Int]): List[C] = xs.map(((i: Int, j: Int) => C.apply(i, j)).curried).map(_(42))
}
object Test extends Test with App {
  assert(mapped(List(52)) == List(B(52)))
  assert(cross(List(27->42)) == List(C(27, 42)))
  assert(f(List(27->42)) == List(C(27, 42)))
  assert(g(List(27)) == List(C(27, 42)))
}
