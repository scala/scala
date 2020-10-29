package tastytest

object TestAlphaAnnot {
  def test1 = AlphaAnnot.++  // error
  def test2 = AlphaAnnot.foo // ok
}
