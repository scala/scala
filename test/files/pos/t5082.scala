trait Something[T]
object Test { class A }
case class Test() extends Something[Test.A]

object User {
  val Test() = Test()
}

object Wrap {
  trait Something[T]
  object Test { class A }
  case class Test(a: Int, b: Int)(c: String) extends Something[Test.A]
  val Test(x, y) = Test(1, 2)(""); (x + y).toString
}
