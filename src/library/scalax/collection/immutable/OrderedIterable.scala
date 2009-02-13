package scalax.collection.immutable

import generic._
import annotation.unchecked.uncheckedVariance

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>SizedIterable</code>.
 *
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait OrderedIterable[+A] extends immutable.Iterable[A]
                             with OrderedIterableTemplate[OrderedIterable, A @uncheckedVariance]
                             with collection.OrderedIterable[A]

object OrderedIterable extends IterableFactory[OrderedIterable]
                          with EmptyIterableFactory[OrderedIterable] {
  val empty: OrderedIterable[Nothing] = Nil
}


