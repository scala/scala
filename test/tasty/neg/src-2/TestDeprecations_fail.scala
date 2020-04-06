package tastytest

object TestDeprecations {

  def test1 = new Deprecations.Old().unused() // error: unused is deprecated
  def test2 = new Deprecations.Old().completelyUseless() // error: completelyUseless is deprecated: this is useless
  // error: match in annotation of method matchAnnot
  // error: block in annotation of method classAnnot
}
