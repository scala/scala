package tastytest

import scala.annotation.experimental

@experimental
object TestTrackedParam {

  import TrackedParam._

  val y: C { type T = Int } = ???

  def test: Int = new F(y).x.t // error: just ignore tracked flag - so x is not refined.
}

