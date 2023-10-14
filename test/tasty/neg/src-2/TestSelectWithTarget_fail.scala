package tastytest

object TestSelectWithTarget {

  // We error when an annotation selects a method overloaded with a targetAnnot
  // until we can erase them correctly. e.g. the annotation arguments may be
  // reflected by a macro into real trees.

  // here it is necessary to call a macro to force the `defAnnot` annotation,
  // alternatively we could use the `@deprecated` annotation as the compiler
  // will analyse that, but this illustrates that most annotations can be
  // harmless unless we specificially need to analyse them.
  def test = TestSelectWithTargetPre.forceAnnots[SelectWithTarget.type, SelectWithTarget.defAnnot]


  def test2 = TestSelectWithTargetPre.forceAnnots[SelectWithTarget.MultiParamsAnnot, SelectWithTarget.mctorAnnot]

  def test3 = SelectWithTarget.MultiParamsAnnotTpe.foo

}
