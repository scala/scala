package tastytest

import ThisTypes._

object TestThisTypes {

  def test = {
    val a = new Sub3()
    val b = new Sub3()

    var aBase = a.doTest.get
    aBase = b.doTest.get // error
  }

}
