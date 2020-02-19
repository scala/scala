package tastytest

import HKNest._

object TestHKNest {
  def test14 = new HKClass_14[Quuux].foo(new Quuux[QuxArg]) // error: diverging lamdba type bounds
  def test15 = new HKClass_14[Quuuux].foo(new Quuuux[DummyQuxArg]) // error: diverging lamdba type bounds
}
