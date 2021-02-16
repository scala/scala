package tastytest

object TestDefAnnots {

  def test1 = DefAnnots.withArgAnnotThrow(1) == 1 // error
  def test2 = DefAnnots.withArgAnnotLoop(1) == 1 // error
  def test3 = DefAnnots.withArgAnnotAssign(1) == 1 // error
  def test4 = DefAnnots.withArgAnnotLambda(1) == 1 // error
  def test5 = DefAnnots.withArgAnnotInlined(1) == 1 // ok - arguments to annotations are not forced
  def test6 = DefAnnots.withArgAnnotSelectExtension(1) == 1 // error

}
