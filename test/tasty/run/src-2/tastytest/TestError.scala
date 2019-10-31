package tastytest

object TestError extends Suite("TestError") {

  final class ConcreteError extends AbsError("ConcreteError")

  val handler: PartialFunction[Throwable, String] = {
    case e: ConcreteError => e.getMessage()
  }

  test(assert((try throw new ConcreteError catch handler) === "ConcreteError"))

}
