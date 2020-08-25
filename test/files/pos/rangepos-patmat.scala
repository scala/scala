// scalac: -Yrangepos
class Foo {
  def test: PartialFunction[Any, String] = { case _ => "ok" }

}
