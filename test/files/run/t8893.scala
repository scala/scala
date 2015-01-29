import annotation.tailrec

object Test {
  def a(): Option[String] = Some("a")

  def test1: Any = {
    a() match {
      case Some(b1) =>
        a() match {
          case Some(b2) =>
            @tailrec
            def tick(i: Int): Unit = if (i < 0) () else tick(i - 1)
            tick(10000000) // testing that this doesn't SOE
          case None => None
        }
      case None => None
    }
  }

  def test2: Any = {
    a() match {
      case Some(b1) =>
        a() match {
          case Some(b2) =>
            @tailrec
            def tick(i: Int): Unit = if (i < 0) () else tick(i - 1)
            tick(10000000) // testing that this doesn't SOE
          case None => test1
        }
      case None =>
        test1 // not a tail call
        test1
    }
  }

  def main(args: Array[String]) {
    test1
    test2
  }
}
