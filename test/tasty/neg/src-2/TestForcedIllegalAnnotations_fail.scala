package tastytest

object TestForcedIllegalAnnotations {
  def test1 =
    new ForcedIllegalAnnotations.Match().forcedMatchInAnnot() // error: match expression in annotation arguments

  def test2 =
    new ForcedIllegalAnnotations.Block().forcedBlockInAnnot() // error: block expression in annotation arguments.
}
