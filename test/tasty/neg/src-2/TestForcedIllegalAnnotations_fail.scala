package tastytest

object TestForcedIllegalAnnotations {
  def test1 = new ForcedIllegalAnnotations.Match() // error: Unsupported Scala 3 match expression in an annotation of method forcedMatchInAnnot
  def test2 = new ForcedIllegalAnnotations.Block() // error: Unsupported Scala 3 block expression in an annotation of method forcedBlockInAnnot
}
