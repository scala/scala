package fix

import strawman.collection.Iterable
import strawman.collection.immutable.List
object Collectionstrawman_v0_Traversable {
  def foo(xs: Iterable[(Int, String)], ys: List[Int]): Unit = {
    xs.to(List)
    xs.to(Set)
    ys.toSeq
    xs.to(List)
    xs.to(strawman.collection.immutable.Set)
    xs.to(strawman.collection.Map)
    xs.iterator()
    ys.iterator()
  }
}
