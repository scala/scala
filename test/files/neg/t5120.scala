class Cell[T](x0: T) {
  type U = T
  var x1: U = x0
}

object Test {
  val str: Cell[String] = new Cell("a")
  val other: Cell[Int]  = new Cell(0)

  def main(args: Array[String]): Unit = {
    List(str, other) foreach (_.x1 = new AnyRef)
    str.x1.length
  }
}
// another way demonstrating the same underlying problem, as reported by Roman Kalukiewicz

class Holder[_T](_f1 : _T, _f2 : _T) {
  type T = _T
  var f1 : T = _f1
  var f2 : T = _f2
}
object Test2 {
  val str = new Holder("t1", "t2")
  val num = new Holder(1, 2)
  List(str, num).foreach(h => h.f1 = new Thread())
  def main(args: Array[String]) {
    println(str.f1)
  }
}
