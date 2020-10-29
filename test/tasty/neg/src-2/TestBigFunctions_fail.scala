package tastytest

object TestBigFunctions {
  val test1 = BigFunctions.bigfun
  val test2 = new BigFunctions.BigFunBox[BigFunctions.bigfun.type]
}
