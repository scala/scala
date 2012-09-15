import language._

object Test extends App {

  case class R[+T](s: String) { def x() = println(s) }

  // Implicits in contention; StringR is nested to avoid ambiguity
  object R { implicit val StringR = R[String]("A") }
  implicit val Default = R[Any]("B")

  class B() extends Dynamic {
    def selectDynamic[T](f: String)(implicit r: R[T]): Unit = r.x()
  }

  val b = new B()

  // These should all produce the same output, but they don't
  b.selectDynamic[String]("baz")
  b.baz[String]
  val c = b.selectDynamic[String]("baz")
  val d = b.baz[String]
}
