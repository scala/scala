// scalac: -Xfatal-warnings

trait C
class Test {
  def test(c: Seq[Option[Int]]) = c.collect { case _: C => }
}
