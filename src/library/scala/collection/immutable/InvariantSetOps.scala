package scala
package collection
package immutable

import scala.language.higherKinds

trait InvariantSetOps[A, +CC[X] <: InvariantSetOps[X, CC, _] with Set[X], +C <: InvariantSetOps[A, CC, C] with CC[A]]
  extends collection.InvariantSetOps[A, CC, C]
     with SetOps[A, Set, C] {
  /** Creates a new set with an additional element, unless the element is
    *  already present.
    *
    *  @param elem the element to be added
    *  @return a new set that contains all elements of this set and that also
    *          contains `elem`.
    */
  def incl(elem: A): C

  /** Alias for `incl` */
  @deprecatedOverriding("This method should be final, but is not due to scala/bug#10853", "2.13.0")
  override /*final*/ def + (elem: A): C = incl(elem) // like in collection.Set but not deprecated

  override def concat(that: collection.Iterable[A]): C = {
    var result: C = coll
    val it = that.iterator
    while (it.hasNext) result = (result.incl(it.next()))
    result
  }
}
