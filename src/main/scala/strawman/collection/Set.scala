package strawman
package collection

import scala.{Any, Boolean, Equals, Int}
import scala.util.hashing.MurmurHash3

trait Set[A]
  extends Iterable[A]
    with SetLike[A, Set]

trait SetLike[A, +C[X] <: Set[X]]
  extends SetOps[A]
    with Equals {

  protected def coll: C[A]

  def canEqual(that: Any) = true

  override def equals(that: Any): Boolean =
    that match {
      case set: Set[A] =>
        (this eq set) ||
        (set canEqual this) &&
        (coll.size == set.size) &&
        (this subsetOf set)
      case _ => false
    }

  override def hashCode(): Int = Set.setHash(coll)

}

trait SetOps[A] extends Any {

  protected def coll: Set[A]

  def subsetOf(that: Set[A]): Boolean

}

// Temporary, TODO move to MurmurHash3
object Set {

  def setHash(xs: Set[_]): Int = unorderedHash(xs, "Set".##)

  final def unorderedHash(xs: Iterable[_], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    xs foreach { x =>
      val h = x.##
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h = seed
    h = MurmurHash3.mix(h, a)
    h = MurmurHash3.mix(h, b)
    h = MurmurHash3.mixLast(h, c)
    MurmurHash3.finalizeHash(h, n)
  }

}