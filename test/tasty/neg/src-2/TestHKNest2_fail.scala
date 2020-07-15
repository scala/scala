package tastytest

import HKNest._

object TestHKNest2 {
  def test16 = new HKClass_15().foo(List(1,2,3)) // error: Union type not supported
  def test17 = new HKClass_17[Option]().foo(Option(1)) // error: Union type not supported
}
