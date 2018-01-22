package fix

import scala.language.postfixOps
object Collectionstrawman_v0_Tuple2Zipped {
  def zipped(xs: List[Int], ys: List[Int]): Unit = {
    xs.lazyZip(ys)
    xs.lazyZip(ys)
    (xs.lazyZip(ys) )
    ((xs).lazyZip((ys)))
    xs.lazyZip(// foo
      ys)
    /* a *//* b */ xs /* c */.lazyZip(/* d */ ys /* e */)/* f *//* g *//* h */
    coll(1).lazyZip(coll(2))
    List(1, 2, 3).lazyZip(LazyList.from(1))
  }
  def coll(x: Int): List[Int] = ???
}

object Collectionstrawman_v0_Tuple3Zipped {
  def zipped(xs: List[Int], ys: List[Int], zs: List[Int]): Unit = {
    xs.lazyZip(ys).lazyZip(zs)
    xs.lazyZip(ys).lazyZip(zs)
    (xs.lazyZip(ys).lazyZip(zs) )
    ((xs).lazyZip((ys)).lazyZip((zs)))
    xs.lazyZip(// foo
      ys).lazyZip(// bar
      zs)
    /* a *//* b */ xs /* c */.lazyZip(/* d */ ys /* e */).lazyZip(/* f */ zs /* g */)/* h *//* i *//* j */
    coll(1).lazyZip(coll(2)).lazyZip(coll(3))
    List(1, 2, 3).lazyZip(Set(1, 2, 3)).lazyZip(LazyList.from(1))
  }
  def coll(x: Int): List[Int] = ???
}