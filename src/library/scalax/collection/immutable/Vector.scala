package scalax.collection.immutable

import generic.covariant

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>SizedIterable</code>.
 *    // !!! todo: insert good immutable vector implementation here.
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Vector[+A] extends Sequence[A]
                    with covariant.VectorTemplate[Vector, A]
                    with collection.Vector[A]

object Vector extends covariant.SequenceFactory[Vector] {
  val empty: Vector[Nothing] = immutable.Vector.empty
}


