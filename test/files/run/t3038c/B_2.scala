
class B extends A {
  lazy val b0 = 71
  lazy val b1 = 72
  lazy val b2 = 73
  lazy val b3 = 74
  lazy val b4 = 75
  lazy val b5 = 76
  lazy val b6 = 77
  lazy val b7 = 78
  lazy val b8 = 79
  lazy val b9 = 80
  override def run = {
    super.run
    println(List(b0, b1, b2, b3, b4, b5, b6, b7, b8, b9))
  }

}

object Test {
  def main(args: Array[String]) {
    new B().run
  }
}
