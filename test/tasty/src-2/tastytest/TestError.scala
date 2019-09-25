package tastytest

object TestError {
  def test1 = assert((try throw new ConcreteError catch handler) === "ConcreteError")

  val handler: PartialFunction[Throwable, String] = {
    case e: ConcreteError => e.getMessage()
  }

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}