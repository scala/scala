package tastytest

import HKNest._

object TestHKNest {
  def test13 = new HKClass_13[HKLam].foo[Int,String](("",0)) // error: unsupported curried type application
  def test16 = new HKClass_16[List].foo[HKLam,Int,String](("",0) :: Nil)
}
