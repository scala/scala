package strawman
package collection

import scala.{Any, Boolean, Equals, Int, inline}
import scala.util.hashing.MurmurHash3

/** Base trait for set collections.
  *
  * A set is a collection that contains no duplicate elements.
  *
  * @author Martin Odersky
  * @author Aleksandar Prokopec
  * @since 2.9
  */
trait Set[A]
  extends Iterable[A]
    with SetLike[A, Set]

/** Base trait for set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends IterableLike[A, C]
    with SetMonoTransforms[A, C[A]]
    with SetPolyTransforms[A, C]
    with (A => Boolean)
    with Equals {

  protected def coll: C[A]

  def contains(elem: A): Boolean

  /** Tests if some element is contained in this set.
    *
    *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
    *  @param elem the element to test for membership.
    *  @return  `true` if `elem` is contained in this set, `false` otherwise.
    */
  @inline final def apply(elem: A): Boolean = this.contains(elem)

  def subsetOf(that: Set[A]): Boolean = this.forall(that)

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

/** Monomorphic transformation operations */
trait SetMonoTransforms[A, +Repr]
  extends IterableMonoTransforms[A, Repr] {

  def & (that: Set[A]): Repr = this.filter(that)

  /** The empty set of the same type as this set
    * @return  an empty set of type `Repr`.
    */
  def empty: Repr

}

trait SetPolyTransforms[A, +C[X]] extends IterablePolyTransforms[A, C] {

  def ++ (that: Set[A]): C[A]

}

object Set extends IterableFactory[Set] {

  def newBuilder[A]: mutable.Builder[A, Set[A]] = immutable.Set.newBuilder
  def empty[A <: Any]: Set[A] = immutable.Set.empty
  def fromIterable[E](it: Iterable[E]): Set[E] =
    it match {
      case s: Set[E] => s
      case _         => newBuilder[E].++=(it).result
    }

  // Temporary, TODO move to MurmurHash3
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