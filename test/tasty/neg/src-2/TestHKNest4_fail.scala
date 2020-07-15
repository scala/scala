package tastytest

import HKNest._

object TestHKNest {
  def test6 = new HKClass_6[Quux].foo(new Quux[QuxArg])
  def test7 = new HKClass_6[Quux].foo(new Quux[Arg1]) // error
}
