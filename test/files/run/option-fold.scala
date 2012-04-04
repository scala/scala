object Test {
  sealed class A
  case object B extends A
  case class C(x: Int) extends A
  
  def f[T](x: Option[T]) = x.fold(List.empty[T])(List(_))
  def g(x: Option[A]) = x.fold(-1) {
    case B    => 0
    case C(x) => x
  }

  def main(args: Array[String]): Unit = {
    println(f(None))
    println(f(Some(5)))
    println(g(None))
    println(g(Some(B)))
    println(g(Some(C(1))))
  }
}
