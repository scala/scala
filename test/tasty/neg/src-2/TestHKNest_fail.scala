package tastytest

import HKNest._

object TestHKNest {

  def test13 = assert(new HKClass_13[HKLam].foo[Int,String](("",0)) == "(,0)") // error: unsupported curried type application

}
