package tastytest

class Box[A](val a: A) extends Product1[A] {
  def _1: A = a
  def canEqual(that: Any): Boolean = that.isInstanceOf[Box[?]]
  override val productPrefix = "Box"
}

object Box {
  def apply[A](a: A) = new Box(a)
}
