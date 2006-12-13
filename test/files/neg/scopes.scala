case class test0(x: int, x: float)

object test1 {
  val x: int = 0
  val x: float = .0f
  {
     val y: int = 0
     val y: float = .0f
     ()
  }
  def params(x: int, x: float) = x
  def curried(x: int)(y: int, y: float) = x + y
  (x: int, x: float) => x
}
