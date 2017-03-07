package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Int, Ordering}

/** Base type of bitsets */
trait BitSet
  extends SortedSet[Int]
    with BitSetLike[BitSet]

/** Base implementation type of bitsets */
trait BitSetLike[+C <: BitSet]
  extends SortedSetLike[Int, SortedSet]
    with BitSetMonoTransforms[C] {

  final def ordering: Ordering[Int] = Ordering.Int

}

trait BitSetMonoTransforms[+C <: BitSet]
  extends SetMonoTransforms[Int, C] {

  /**
    * Computes the symmetric difference of this bitset and another bitset by performing a bitwise “exclusive-or”.
    *
    * @param other the other bitset to take part in the symmetric difference.
    * @return a bitset containing those bits of this bitset or the other bitset that are not contained in both bitsets.
    */
  def ^ (other: BitSet): C

  /**
    * Builds a new bitset by applying a function to all elements of this bitset
    * @param f the function to apply to each element.
    * @return a new bitset resulting from applying the given function ''f'' to
    *         each element of this bitset and collecting the results
    */
  def map(f: Int => Int): C

}
