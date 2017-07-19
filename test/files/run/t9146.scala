trait A {
  val value: String
}

trait B {
  val as: List[A]
}

case class C(value: String) extends A

object Test {
  object test extends B {
    val as = List(
      new C("one") {},
      new C("two") {}
    )

    def method = as match {
      case List(C("one"), _) => 1
      case _ => 42
    }
  }
  def main(args: Array[String]): Unit = {
    assert(test.method == 1)
  }
}
