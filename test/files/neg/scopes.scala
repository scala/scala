case class test0(x: int, x: float)

object test1 {
  type t = int
  type t = float
  val x: int = 0
  val x: float = .0f;
  {
    val y: int = 0
    val y: float = .0f
    ()
  }
  def f1(x: int, x: float) = x
  def f2(x: int)(y: int, y: float) = x + y
  val closure = (x: int, x: float) => x
  List() match {
    case x::x => x
    case Nil => Nil
  }
}
