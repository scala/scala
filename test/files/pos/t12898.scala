// scalac: -deprecation -Werror

import annotation.nowarn

class T {
  @nowarn("msg=unchecked since it is eliminated by erasure")
  @nowarn("msg=Pair in object Predef")
  def f(x: Any): Int = x match {
    case l: List[Int] => l.head
    case _ =>
      Pair(1, 2)._2
  }
}
