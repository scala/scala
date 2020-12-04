package tastytest

object TestTargetNameAnnot {
  def test1 = TargetNameAnnot.++  // error
  def test2 = TargetNameAnnot.foo // ok
}
