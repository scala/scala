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
trait Iterable[+A] extends collection.Iterable[A]
                      with covariant.IterableTemplate[Iterable, A]

object Iterable extends covariant.IterableFactory[Iterable] {
  val empty: Iterable[Nothing] = Nil
}


