class C { case class Foo private (x: Int); Foo.apply(0) }

object Test {
  def test(c: C) = {import c.Foo; Foo.apply(0)}
  def main(args: Array[String]): Unit = {
    test(new C)
  }  
}
