package strawman
package collection
package immutable
import strawman.collection.mutable.{Builder, ImmutableBuilder}

import strawman.collection.decorators.ImmutableMapDecorator

/**
  * An immutable multiset
  * @tparam A the element type of the collection
  */
class MultiSet[A] private (elems: Map[A, Int])
  extends collection.MultiSet[A]
    with Iterable[A]
    with collection.MultiSetOps[A, MultiSet, MultiSet[A]] {

  def occurrences: Map[A, Int] = elems

  def iterableFactory: IterableFactory[MultiSet] = MultiSet
  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): MultiSet[A] = fromIterable(coll)
  protected[this] def newSpecificBuilder(): Builder[A, MultiSet[A]] = iterableFactory.newBuilder()

  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one more occurrence of `elem`
    * @param elem the element to add
    */
  def incl(elem: A): MultiSet[A] =
    new MultiSet(elems.updatedWith(elem) {
      case None    => Some(1)
      case Some(n) => Some(n + 1)
    })

  /** Alias for `incl` */
  @`inline` final def + (elem: A): MultiSet[A] = incl(elem)

  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one occurrence less of `elem`
    *
    * @param elem the element to remove
    */
  def excl(elem: A): MultiSet[A] =
    new MultiSet(elems.updatedWith(elem) {
      case Some(n) => if (n > 1) Some(n - 1) else None
    })

  /** Alias for `excl` */
  @`inline` final def - (elem: A): MultiSet[A] = excl(elem)

}

object MultiSet extends IterableFactory[MultiSet] {

  def from[A](source: IterableOnce[A]): MultiSet[A] =
    source match {
      case ms: MultiSet[A] => ms
      case _ => (newBuilder[A]() ++= source).result()
    }

  def empty[A] = new MultiSet[A](Map.empty)

  def newBuilder[A](): Builder[A, MultiSet[A]] =
    new ImmutableBuilder[A, MultiSet[A]](empty[A]) {
      def add(elem: A): this.type = { elems = elems + elem; this }
    }

}
