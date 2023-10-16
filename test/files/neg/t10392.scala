
//> using options -feature -Werror -Wunused

// warn when conversion method is present but unused

class C {
  implicit val cv0: List[String] = List("zero", "one", "two", "three")
  //implicit val cv1: Int => String = _.toString
  private implicit def cv2(i: Int): String = i.toString * 2

  def f(i: Int): String = i
}

object Test extends App {
  val c = new C
  println {
    c.f(3)
  }
}
