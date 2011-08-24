object Foo {
  def test(s: String) = {
    (s: Seq[Char]) match {
        case Seq('f', 'o', 'o', ' ', rest1 @ _*) =>
          rest1
        case Seq('b', 'a', 'r', ' ', ' ', rest2 @ _*) =>
          rest2
        case _ =>
          s
    }
  }
}