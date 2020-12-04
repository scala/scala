package tastytest

import Infix.BoxedInt

object TestInfix {

  val x = BoxedInt(0)
  val y = BoxedInt(2)

  def test = assert((x min y) == x)

}
