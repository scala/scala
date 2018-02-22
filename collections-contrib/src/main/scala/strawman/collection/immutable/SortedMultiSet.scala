package strawman
package collection
package immutable

import strawman.collection.decorators.ImmutableMapDecorator
import strawman.collection.mutable.{Builder, ImmutableBuilder}

/**
  * An immutable multiset whose elements are sorted.
  * @tparam A Type of elements
  */
class SortedMultiSet[A] private (elems: SortedMap[A, Int])(implicit val ordering: Ordering[A])
  extends MultiSet[A]
    with collection.SortedMultiSet[A]
    with MultiSetOps[A, MultiSet, SortedMultiSet[A]]
    with collection.SortedMultiSetOps[A, SortedMultiSet, SortedMultiSet[A]]
    with collection.IterableOps[A, MultiSet, SortedMultiSet[A]] {

  def occurrences: SortedMap[A, Int] = elems

  override def iterableFactory: IterableFactory[MultiSet] = MultiSet
  override def sortedIterableFactory: SortedIterableFactory[SortedMultiSet] = SortedMultiSet

  def rangeImpl(from: Option[A], until: Option[A]): SortedMultiSet[A] =
    new SortedMultiSet(elems.rangeImpl(from, until))

  /**
    * @return an immutable sorted multiset containing all the elements of
    *         this multiset and one more occurrence of `elem`
    * @param elem the element to add
    */
  def incl(elem: A): SortedMultiSet[A] =
    new SortedMultiSet(elems.updatedWith(elem) {
      case None    => Some(1)
      case Some(n) => Some(n + 1)
    })

  /**
    * @return an immutable sorted multiset containing all the elements of
    *         this multiset and one occurrence less of `elem`
    *
    * @param elem the element to remove
    */
  def excl(elem: A): SortedMultiSet[A] =
    new SortedMultiSet(elems.updatedWith(elem) {
      case Some(n) => if (n > 1) Some(n - 1) else None
    })
}

object SortedMultiSet extends SortedIterableFactory[SortedMultiSet] {

  def from[A: Ordering](source: IterableOnce[A]): SortedMultiSet[A] =
    source match {
      case sms: SortedMultiSet[A] => sms
      case _ => (newBuilder[A]() ++= source).result()
    }

  def empty[A: Ordering]: SortedMultiSet[A] = new SortedMultiSet[A](TreeMap.empty)

  def newBuilder[A: Ordering](): Builder[A, SortedMultiSet[A]] =
    new ImmutableBuilder[A, SortedMultiSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }

}