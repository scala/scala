package tastytest

import lib.TpeAnnotated

object TestTpeAnnotated extends scala.App {

  val f: Array[Int] = TpeAnnotated.f
  val foo1: Int = TpeAnnotated.foo1[Int]()
  val foo3: TpeAnnotated#C#D = TpeAnnotated.foo3()

}
