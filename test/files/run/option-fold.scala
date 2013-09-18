object Test {
  sealed class A
  case object B extends A
  case class C(x: Int) extends A

  def f[T](x: Option[T]) = x.fold(List.empty[T])(List(_))
  def g(x: Option[A]) = x.fold(-1) {
    case B    => 0
    case C(x) => x
    case _    => ???
  }

  def main(args: Array[String]): Unit = {
    println(f(None))        //List()
    println(f(Some(5)))     //List(5)
    println(g(None))        //-1
    println(g(Some(B)))     //0
    println(g(Some(C(1))))  //1
  }
}
