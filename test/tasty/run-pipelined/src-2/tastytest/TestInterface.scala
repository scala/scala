package tastytest

import lib.Interface

object TestInterface extends scala.App {

  val ii = Interface.create[Int](23)

  assert((ii.getField: Int) == 23)
  assert((ii.fieldHash: Int) == 23)

  val const: 42L = Interface.CONST
  assert(const == 42L)

  class MyInterface extends Interface[Int] {
    def getField: Int = 48
  }

  val myii = new MyInterface
  assert((myii.getField: Int) == 48)
  assert((myii.fieldHash: Int) == 48)

  class MyInterface2 extends Interface[Int] {
    def getField: Int = 48
    override def fieldHash: Int = 49
  }

  val myii2 = new MyInterface2
  assert((myii2.getField: Int) == 48)
  assert((myii2.fieldHash: Int) == 49)
}
