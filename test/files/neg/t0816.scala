abstract class Atest(val data: String)

case class Btest(override val data: String, val b: Boolean) extends Atest(data)

case class Ctest(override val data: String) extends Btest(data, true)

class testCaseClass {
  def test(x: Atest) = x match {
    case Ctest(data) => Console.println("C")
    case Btest(data, b) => Console.println("B")
  }
}
