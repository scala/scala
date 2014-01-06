object Test {

  abstract class Number
  case class MyInt(n: Int) extends Number
  case class MyDouble(d: Double) extends Number

  trait Term[a]
  case class Cell[a](var x: a) extends Term[a]
  final case class NumTerm(val n: Number) extends Term[Number]

  def f[a](t: Term[a], c: Cell[a]) {
    t match {
      case NumTerm(n) => c.x = MyDouble(1.0)
    }
  }

  val x: Term[Number] = NumTerm(MyInt(5))

  def main(args: Array[String]) {
    val cell = Cell[Number](MyInt(6))
    Console.println(cell)
    f[Number](new NumTerm(MyInt(5)), cell)
    Console.println(cell)
  }
}
