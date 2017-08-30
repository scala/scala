package fix

import strawman.collection.Iterable
import strawman.collection.immutable.{ List, Set }
object Collectionstrawman_v0_Traversable {
  def foo(xs: Iterable[(Int, String)], ys: List[Int]): Unit = {
    xs.to(strawman.collection.immutable.List)
    xs.to(strawman.collection.immutable.Set)
    ys.toSeq
    xs.to(List)
    xs.to(Set)
    xs.to(strawman.collection.Map)
    xs.iterator()
    ys.iterator()
  }
}
