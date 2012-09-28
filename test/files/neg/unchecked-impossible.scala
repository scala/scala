final case class T2[+A, +B](a: A, b: B)

class A {
  def f1 = T2(1, 2) match {
    case Seq(x) =>
    case _      =>
  }
  def f2 = T2(1, 2) match {
    case _: T2[Int, Int] => /* nowarn */
    case _               =>
  }
  def f3 = T2(1, 2) match {
    case _: T2[_, Int] => /* nowarn */
    case _             =>
  }
}
