object test1 {
  type t = Int
  type t = Float
  val x: Int = 0
  val x: Float = .0f;
  {
    val y: Int = 0
    val y: Float = .0f
    ()
  }
  def f1(x: Int, x: Float) = x
  def f2(x: Int)(y: Int, y: Float) = x + y
  val closure = (x: Int, x: Float) => x
  List() match {
    case x::x => x
    case Nil => Nil
  }
}
