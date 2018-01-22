/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

object Collectionstrawman_v0_Traversable {
  def foo(xs: Traversable[(Int, String)], ys: List[Int]): Unit = {
    xs.to[List]
    xs.to[Set]
    xs.toIterator
    ys.iterator
  }
}
