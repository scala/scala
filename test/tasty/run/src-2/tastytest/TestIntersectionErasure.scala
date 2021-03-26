package tastytest

import IntersectionErasure.{universe => u}

object TestIntersectionErasure extends Suite("TestIntersectionErasure") {

  test {
    val sam: u.IntersectionSAM = x => x
    assert(sam(u.EmptyTree) === (u.EmptyTree: u.TreeShimSAM))
  }

}
