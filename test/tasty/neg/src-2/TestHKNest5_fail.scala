package tastytest

import HKNest._

object TestHKNest {
  trait Show[-T]
  def test4 = new HKClass_4[Show]
  def test5 = new HKClass_5[List]
}
