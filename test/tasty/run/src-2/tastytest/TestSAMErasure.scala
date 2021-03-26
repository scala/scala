package tastytest

import SAMErasure._

object TestSAMErasure extends Suite("TestSAMErasure") {

  def f = ((x: TreeShimSAM) => x): FunTreeShimSAM

  def g = ((xs: Array[TreeShimSAM]) => xs): FunTreeShimSAM2

  case object EmptyTree extends TreeShimSAMApi
  val tree = EmptyTree.asInstanceOf[TreeShimSAM]

  test {
    assert(f(tree) == tree)
  }

  test {
    val trees = Array(tree)
    assert(g(trees) == trees)
  }

}
