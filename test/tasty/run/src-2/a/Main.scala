package a

import tastytest.opaques.Offset

final case class A(off: Offset)

object Main {
  def foo(): Unit = {
    assert(A(Offset(10)).off == Offset(10))
  }
}
