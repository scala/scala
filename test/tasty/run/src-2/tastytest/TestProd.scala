package tastytest

object TestProd extends Suite("TestProd") {

  val Some(one) = Prod(1)

  test("product * empty === empty") {
    val Some(product) = Prod(getRandomPos)
    assert(product * Prod.empty === Prod.empty)
  }

  test("product * one === product") {
    val Some(product) = Prod(getRandomPos)
    assert(product * one === product)
  }

  test("product mul empty === empty") {
    val Some(product) = Prod(getRandomPos)
    assert((product `mul` Prod.empty) === Prod.empty)
  }

  test("product mul one === product") {
    val Some(product) = Prod(getRandomPos)
    assert((product `mul` one) === product)
  }

  override val reps = 1_000_000
}
