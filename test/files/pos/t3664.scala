
//> using options -Werror -Xlint -Xsource:3-cross

import language.implicitConversions

case class C(i: Int)
object C // no function parent

// use conversion, don't warn about apply insertion
class Test {
  implicit def cv(c: C.type): Function[Int, C] = C(_)
  def f(xs: List[Int]): List[C] = xs.map(C)
}
