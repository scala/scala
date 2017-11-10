/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.language.postfixOps
object Collectionstrawman_v0_Tuple2Zipped {
  def zipped(xs: List[Int], ys: List[Int]): Unit = {
    (xs, ys).zipped
    (xs,ys).zipped
    ((xs, ys) zipped)
    (((xs)    ,    (ys)).zipped)
    (xs, // foo
      ys).zipped
    /* a */(/* b */ xs /* c */, /* d */ ys /* e */)/* f */./* g */zipped/* h */
    (coll(1), coll(2)).zipped
    (List(1, 2, 3), Stream.from(1)).zipped
  }
  def coll(x: Int): List[Int] = ???
}

object Collectionstrawman_v0_Tuple3Zipped {
  def zipped(xs: List[Int], ys: List[Int], zs: List[Int]): Unit = {
    (xs, ys, zs).zipped
    (xs,ys,zs).zipped
    ((xs, ys, zs) zipped)
    (((xs)    ,    (ys)    ,    (zs)).zipped)
    (xs, // foo
      ys, // bar
      zs).zipped
    /* a */(/* b */ xs /* c */, /* d */ ys /* e */, /* f */ zs /* g */)/* h */./* i */zipped/* j */
    (coll(1), coll(2), coll(3)).zipped
    (List(1, 2, 3), Set(1, 2, 3), Stream.from(1)).zipped
  }
  def coll(x: Int): List[Int] = ???
}