object Test {
  object bug425 {
    case class A(x: Int)
    case class B(override val x: Int, y: Double) extends A(x)

    val b: A = B(5, 3.3)
    b match {
      case B(x, y) => Console.println(y)
      case A(x) => Console.println(x)
    }
  }

  object bug816 {
    abstract class Atest(val data: String)

    case class Btest(override val data: String, val b: boolean) extends Atest(data)

    case class Ctest(override val data: String) extends Btest(data, true)

    class testCaseClass {
      def test(x: Atest) = x match {
        case Ctest(data) => Console.println("C")
        case Btest(data, b) => Console.println("B")
      }
    }
  }
}
