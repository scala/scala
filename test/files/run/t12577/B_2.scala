trait X
class B[T](val i: Int)
object B {
  def unapply[T](b: B[T]): Option[Int] = Some(b.i)
}
object Test {
  def main(args: Array[String]): Unit = {
    A.foo[B[X], Int] { case B(x) => x }
  }
}
