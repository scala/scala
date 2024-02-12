package tastytest

import lib.{Boxer, JavaBox}

object TestBasic extends scala.App {
  val box: JavaBox[Int] = Boxer.box(42) // Testing Int <: java.lang.Object when reading from Java TASTy.
  assert((box.value: Int) == 42)
}
