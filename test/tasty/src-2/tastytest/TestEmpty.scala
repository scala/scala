package tastytest

object TestEmpty {

  def test1 = assert(productString(new EmptyProduct) === "EmptyProduct()")

  def productString(p: Product) = ScalaRunTime._toString(p)

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}