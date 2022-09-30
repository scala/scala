
trait Compute[A] {
  type Start
  val start: Compute[Start]
}

object Test {
  def main(args: Array[String]): Unit = foo(new Compute[Unit] { type Start = Unit; val start = this })
  def foo[A](c: Compute[A]): Unit =
    c match {
      case c: Compute[A] =>
        new Compute[A] {
          type Start = c.Start
          val start = c.start
        }
    }
}
