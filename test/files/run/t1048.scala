final case class W[A](v: A)

object E {
  def unapply(w: W[Any]): Option[Any] = None
}

object Bug {
  def bug[A](e: Either[W[_], A]) = e match {
    case Left(E(x)) => 1
    case Right(x)   => 2
    case _          => 3
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(Bug.bug(Left(W(5))))
    println(Bug.bug(Right(5)))
  }
}

