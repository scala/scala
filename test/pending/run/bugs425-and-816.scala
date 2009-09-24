object Test {
  object bug816 {
    abstract class Atest(val data: String)
    case class Btest(override val data: String, val b: Boolean) extends Atest(data)
    case class Ctest(override val data: String) extends Btest(data, true)

    class testCaseClass {
      def test(x: Atest) = x match {
        case Ctest(data)    => "C"
        case Btest(data, b) => "B"
      }
    }
    def go() = {
      val tcc = new testCaseClass()

      tcc.test(Ctest("foo")) + tcc.test(Btest("bar", true))
    }
  }

  object bug425 {
    case class A(x: Int)
    case class B(override val x: Int, y: Double) extends A(x)

    val b: A = B(5, 3.3)
    def flail = b match {
      case B(x, y)  => "B"
      case A(x)     => "A"
    }
    def flail2 = (B(10, 5.5): Any) match {
      case A(20)      => "1"
      case A(10)      => "2"
      case _          => "fail"
    }
    def go() = flail + flail2
  }

  def main(args: Array[String]): Unit = {
    assert(bug816.go() == "CB")
    assert(bug425.go() == "B2")
  }
}
