package tastytest

object TestSelectWithTarget {

  // We error when an annotation selects a
  // method overloaded with a targetAnnot
  // until we can erase them correctly.
  // e.g. the annotation arguments may be
  // reflected by a macro into real trees
  def test = SelectWithTarget.selectFooString

}
