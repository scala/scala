class Test {
  implicit def int = 42
  implicit val string = "hello"
  implicit val annotated: Double = 0
  implicit class X(a: Int)
  implicit var stringVarPlaceholder: String = _
}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val localInt = 42
    implicit def localString = "hello"
    implicit def localAnnotated: Double = 0
    def localFunParam: Unit = { implicit x: Int =>
    }
    implicit object Y extends Test
  }
}

