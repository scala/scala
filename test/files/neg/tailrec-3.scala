import annotation.tailrec

object Test {
  @tailrec private def quux(xs: List[String]): List[String] = quux(quux(xs))
  @tailrec private def quux2(xs: List[String]): List[String] = xs match {
    case x1 :: x2 :: rest => quux2(x1 :: quux2(rest))
    case _                => Nil
  }
  @tailrec private def quux3(xs: List[String]): Boolean = xs match {
    case x :: xs if quux3(List("abc"))  => quux3(xs)
    case _                              => false
  }
}

