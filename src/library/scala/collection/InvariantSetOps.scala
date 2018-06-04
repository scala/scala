package scala
package collection

import scala.language.higherKinds

trait InvariantSetOps[A, +CC[X] <: InvariantSetOps[X, CC, _] with Set[X], +C <: InvariantSetOps[A, CC, C] with CC[A]]
  extends SetOps[A, Set, C] {
  def concat(that: collection.Iterable[A]): C = fromSpecificIterable(new View.Concat(toIterable, that))

  @deprecated("Consider requiring an immutable Set or fall back to Set.union", "2.13.0")
  def + (elem: A): C = fromSpecificIterable(new View.Appended(toIterable, elem))

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  def + (elem1: A, elem2: A, elems: A*): C = fromSpecificIterable(new View.Concat(new View.Appended(new View.Appended(toIterable, elem1), elem2), elems))

  /** Alias for `concat` */
  @`inline` final def ++ (that: collection.Iterable[A]): C = concat(that)

  /** Computes the union between of set and another set.
    *
    *  @param   that  the set to form the union with.
    *  @return  a new set consisting of all elements that are in this
    *  set or in the given set `that`.
    */
  @`inline` final def union(that: collection.Iterable[A]): C = concat(that)

  /** Alias for `union` */
  @`inline` final def | (that: collection.Iterable[A]): C = concat(that)
}
