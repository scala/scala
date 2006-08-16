object Test{

abstract class Number
case class Int(n: int) extends Number
case class Double(d: double) extends Number

trait Term[+a]
case class Cell[a](var x: a) extends Term[a]
case class NumTerm(val n: Number) extends Term[Number]
class IntTerm(n: Int) extends NumTerm(n) with Term[Int]


def f[a](t:Term[a], c:Cell[a]): unit =
  t match {
    case NumTerm(n) => c.x = Double(1.0)
  }


val x:Term[Number] = NumTerm(Int(5))

def main(args: Array[String]): unit = {
  val cell = Cell[Int](Int(6))
  Console.println(cell)
  f[Int](new IntTerm(Int(5)), cell)
  Console.println(cell)
}
}
