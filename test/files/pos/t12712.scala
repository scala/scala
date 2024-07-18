//> using options -Werror
object T {
  private sealed trait T
  private object O extends T
  private trait U extends T
  private object P extends U

  private def t(t: T) = t match {
    case O => ()
    case _: U => println("hai")
  }

  def main(args: Array[String]): Unit = {
    t(P)
  }
}
