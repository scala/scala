package tastytest

/**Suspended as `error: value * is not a member of tastytest#Prod`
 */
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

  override val reps = 1_000_000
}
