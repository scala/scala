package tastytest

import HKNest._

object TestHKNest {

  def test = assert(new HKClass_13[HKLam].foo[Int,String](("",0)) == "(,0)") // error: unsupported curried type application

}
