package fix

import strawman.collection.Iterable
import strawman.collection.immutable.{ List, Set }
object Collectionstrawman_v0_Traversable {
  def foo(xs: Iterable[(Int, String)], ys: List[Int]): Unit = {
    xs.to(List)
    xs.to(Set)
    xs.iterator()
    ys.iterator()
  }
}
