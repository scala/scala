package scalax.collection.immutable

import generic.covariant

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
                      with covariant.SequenceTemplate[Sequence, A]
                      with collection.Sequence[A]

object Sequence extends covariant.SequenceFactory[Sequence] {
  val empty: Sequence[Nothing] = Nil
}


