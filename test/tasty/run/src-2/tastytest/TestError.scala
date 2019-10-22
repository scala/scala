package tastytest

object TestError {

  final class ConcreteError extends AbsError("ConcreteError")

  val handler: PartialFunction[Throwable, String] = {
    case e: ConcreteError => e.getMessage()
  }

  def test1 = assert((try throw new ConcreteError catch handler) === "ConcreteError")

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}
