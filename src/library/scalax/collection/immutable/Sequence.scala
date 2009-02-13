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
 *  @autor   Martin Oderskyter
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Sequence[+A] extends OrderedIterable[A]
                      with SequenceTemplate[Sequence, A @uncheckedVariance]
                      with collection.Sequence[A]

object Sequence extends SequenceFactory[Sequence] with EmptyIterableFactory[Sequence] {
  val empty: Sequence[Nothing] = Nil
}


