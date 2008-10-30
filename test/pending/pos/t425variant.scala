object Temp{
  case class A(x: Int)
  case class B(override val x: Int, y: Double) extends A(x)

  B(5, 3.3) match {
    case B(x, y) => Console.println(y)
    case A(x) => Console.println(x)
  }
}

