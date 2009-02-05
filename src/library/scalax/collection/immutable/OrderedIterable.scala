package scalax.collection.immutable

import generic.covariant

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
trait OrderedIterable[+A] extends Iterable[A]
                             with covariant.OrderedIterableTemplate[OrderedIterable, A]
                             with collection.OrderedIterable[A]

object OrderedIterable extends covariant.IterableFactory[OrderedIterable] {
  val empty: OrderedIterable[Nothing] = Nil
}


