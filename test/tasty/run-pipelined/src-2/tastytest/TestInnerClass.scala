package tastytest

import lib.InnerClass

object TestInnerClass extends scala.App {

  val ici = new InnerClass[Int]()

  val ici_inner = new ici.Inner[Long](23, 47L)

  assert((ici_inner.outerField: Int) == 23)
  assert((ici_inner.innerField: Long) == 47L)

  assert((ici_inner.getOuterField: Int) == 23)
  assert((ici_inner.getInnerField: Long) == 47L)

}
