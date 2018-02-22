package strawman
package collection
package immutable

import strawman.collection.mutable.{Builder, ImmutableBuilder}

import strawman.collection.decorators.ImmutableMapDecorator

/**
  * An immutable multiset
  * @tparam A the element type of the collection
  */
trait MultiSet[A]
  extends collection.MultiSet[A]
    with Iterable[A]
    with MultiSetOps[A, MultiSet, MultiSet[A]] {

  override def iterableFactory: IterableFactory[MultiSet] = MultiSet
}

trait MultiSetOps[A, +CC[X] <: MultiSet[X], +C <: MultiSet[A]] extends collection.MultiSetOps[A, CC, C] {
  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one more occurrence of `elem`
    * @param elem the element to add
    */
  def incl(elem: A): C

  /** Alias for `incl` */
  @`inline` final def + (elem: A): C = incl(elem)

  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one occurrence less of `elem`
    *
    * @param elem the element to remove
    */
  def excl(elem: A): C

  /** Alias for `excl` */
  @`inline` final def - (elem: A): C = excl(elem)
}

class MultiSetImpl[A] private[immutable] (elems: Map[A, Int]) extends MultiSet[A] {

  def occurrences: Map[A, Int] = elems

  override def iterableFactory: IterableFactory[MultiSet] = MultiSet

  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one more occurrence of `elem`
    * @param elem the element to add
    */
  def incl(elem: A): MultiSet[A] =
    new MultiSetImpl(elems.updatedWith(elem) {
      case None    => Some(1)
      case Some(n) => Some(n + 1)
    })

  /**
    * @return an immutable multiset containing all the elements of this multiset
    *         and one occurrence less of `elem`
    *
    * @param elem the element to remove
    */
  def excl(elem: A): MultiSet[A] =
    new MultiSetImpl(elems.updatedWith(elem) {
      case Some(n) => if (n > 1) Some(n - 1) else None
    })

}

object MultiSet extends IterableFactory[MultiSet] {

  def from[A](source: IterableOnce[A]): MultiSet[A] =
    source match {
      case ms: MultiSet[A] => ms
      case _ => (newBuilder[A]() ++= source).result()
    }

  def empty[A] = new MultiSetImpl[A](Map.empty)

  def newBuilder[A](): Builder[A, MultiSet[A]] =
    new ImmutableBuilder[A, MultiSet[A]](empty[A]) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }

}
