package tastytest

object TestAnnotated {
  def test1 = new Annotated {}
  def test2 = new RootAnnotated {}
  def test3 = {
    val o = new OuterClassAnnotated {}
    o.foo
  }
  def test4 = new ParameterizedAnnotated(23).foo
  def test5 = {
    val o = new OuterAnnotated {}
    o.foo
  }
  def test6 = new SelectInAnnotated.AmbiguousAnnotated {}
  def test7 = new SelectInAnnotatedinParent.AmbiguousAnnotated {}
}
