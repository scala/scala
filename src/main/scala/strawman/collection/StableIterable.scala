package strawman.collection

import scala.util.hashing.MurmurHash3

import scala.{Any, Boolean, Equals, Int}

/**
  * Mixin to be used by collections providing a stable
  * traversal order (e.g. Seq).
  *
  * Overrides `equals` to compare the elements of the collections.
  */
trait StableIterable[+A] extends Iterable[A] with Equals {

  /** Do the elements of this `StableIterable` are the same (and in the same order)
    * as those of `that`?
    */
  def sameElements[B >: A](that: StableIterable[B]): Boolean = {
    val these = this.iterator()
    val those = that.iterator()
    while (these.hasNext && those.hasNext)
      if (these.next() != those.next())
        return false
    // At that point we know that *at least one* iterator has no next element
    // If *both* of them have no elements then the collections are the same
    these.hasNext == those.hasNext
  }

  /** Method called from equality methods, so that user-defined subclasses can
    *  refuse to be equal to other collections of the same kind.
    *  @param   that   The object with which this $coll should be compared
    *  @return  `true`, if this $coll can possibly equal `that`, `false` otherwise. The test
    *           takes into consideration only the run-time types of objects but ignores their elements.
    */
  def canEqual(that: Any): Boolean = true

  override def equals(o: scala.Any): Boolean =
    o match {
      case it: StableIterable[A] => (it canEqual this) && sameElements(it)
      case _ => false
    }

  override def hashCode(): Int =
    StableIterable.stableIterableHash(coll)

}

// Temporary: TODO move to MurmurHash3.scala
object StableIterable {

  final def stableIterableHash(xs: StableIterable[_]): Int = {
    var n = 0
    var h = "StableIterable".##
    val it = xs.iterator()
    while (it.hasNext) {
      h = MurmurHash3.mix(h, it.next().##)
      n += 1
    }
    MurmurHash3.finalizeHash(h, n)
  }

}