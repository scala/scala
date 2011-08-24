trait X
trait A {
  def f: Option[X]
  def g: Option[X]
}
object B {
    private class Y extends X { val y = 42 }
}
class B extends A {
  private class Bippy

  override def f = Some(new B.Y)
  override def g: Option[X] = Some(new B.Y)
}

