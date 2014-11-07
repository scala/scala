class A {
  protected def x = 0
  protected[A] def y = 0
}
 
class B extends A {
  override def x = 1
  def superY = super[A].y
  override def y = 1
}


object Test {
  def main(args: Array[String]): Unit = {
    val b = new B
    assert(b.x == 1)
    assert(b.y == 1)
    assert(b.superY == 0)
  }
}
