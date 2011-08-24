object Test {
  case class A(x: Int)(y: Int)(z: String)

  def f(x: Any) = x match {
    case A(x)   => x
    case _      => -1
  }

  def main(args: Array[String]): Unit = {
    println(f(A(10)(20)("abc")))
    println(f(A(-10)(20)("abc")))
    println(f(List(1)))
  }
}
