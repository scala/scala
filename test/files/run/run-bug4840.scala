object Test {
  def g(x: Boolean): Option[String] = if (x) Some("booya") else None

  def f1() = {
    for (x <- g(true)) yield {
      g(false) match {
        case Some(_) => sys.error("")
        case None    => 5
      }
    }
  }

  def f2() = {
    for (x <- g(true) ; y <- g(true) ; z <- g(true)) yield {
      for (x <- g(true) ; y <- g(true) ; z <- g(true)) yield {
        g(true) map { _ =>
          (null: Any) match {
            case Some(x: Int) => x
            case _            => 5
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(f1())
    println(f2())
  }
}
