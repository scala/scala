case class Foo(a: String, b: String)

object Bar {
  def unapply(s: String): Option[Long] =
    try { Some(s.toLong) } catch { case _ => None }
}

object Test {
  def main(args: Array[String]) {
    val f = Foo("1", "2")
    f match {
      case Foo(Bar(1), Bar(2)) => 1
      case Foo(Bar(i), Bar(j)) if i >= 0 => 2
      case _ => 3
    }
  }
}

