package tastytest

object TestApplySigPoly {

  // force an annotation tree with an APPLYsigpoly node
  def test = TestApplySigPolyPre.forceAnnots[ApplySigPoly.type, ApplySigPoly.boxAnnot]

}
