package tastytest

object Testderiving extends Suite("Testderiving") {

  test(assert(deriving.EmptyProduct.productArity == 0))

}
