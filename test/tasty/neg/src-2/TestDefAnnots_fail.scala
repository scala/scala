package tastytest

object TestDefAnnots {

  def test1 = DefAnnots.withArgAnnotThrow(1) == 1
  def test2 = DefAnnots.withArgAnnotLoop(1) == 1
  def test3 = DefAnnots.withArgAnnotAssign(1) == 1
  def test4 = DefAnnots.withArgAnnotLambda(1) == 1
  def test5 = DefAnnots.withArgAnnotInlined(1) == 1

}
