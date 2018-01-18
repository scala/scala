package strawman
package collection
package mutable

import strawman.collection.decorators.MutableMapDecorator

/**
  * A mutable multiset.
  */
class MultiSet[A] private (val elems: Map[A, Int])
  extends collection.MultiSet[A]
    with collection.MultiSetOps[A, MultiSet, MultiSet[A]]
    with Growable[A]
    with Shrinkable [A] {

  def iterableFactory: IterableFactory[MultiSet] = MultiSet
  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): MultiSet[A] = fromIterable(coll)
  protected[this] def newSpecificBuilder(): Builder[A, MultiSet[A]] = iterableFactory.newBuilder()

  def occurrences: collection.Map[A, Int] = elems

  def addOne(elem: A): this.type = {
    elems.updateWith(elem) {
      case None    => Some(1)
      case Some(n) => Some(n + 1)
    }
    this
  }

  def subtractOne(elem: A): this.type = {
    elems.updateWith(elem) {
      case Some(n) => if (n > 1) Some(n - 1) else None
    }
    this
  }

  def clear(): Unit = elems.clear()
}

object MultiSet extends IterableFactory[MultiSet] {

  def from[A](source: IterableOnce[A]): MultiSet[A] = (newBuilder[A]() ++= source).result()

  def empty[A]: MultiSet[A] = new MultiSet[A](Map.empty)

  def newBuilder[A](): Builder[A, MultiSet[A]] = new GrowableBuilder[A, MultiSet[A]](empty)

}