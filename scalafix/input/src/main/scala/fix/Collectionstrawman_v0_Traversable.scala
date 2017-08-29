/*
rewrite = "scala:fix.Collectionstrawman_v0"
 */
package fix

object Collectionstrawman_v0_Traversable {
  def foo(xs: Traversable[(Int, String)], ys: List[Int]): Unit = {
    xs.toList
    xs.toSet
    ys.toSeq
    xs.to[List]
    xs.to[Set]
    xs.toMap
    xs.toIterator
    ys.iterator
  }
}
