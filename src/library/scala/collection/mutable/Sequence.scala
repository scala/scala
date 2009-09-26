/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A subtrait of <code>collection.Sequence</code> which represents sequences
 *  that can be mutated.
 *  The class adds an <code>update</code> method to <code>collection.Sequence</code>.
 *
 *  @since 2.8
 */
trait Sequence[A] extends Iterable[A]
                     with scala.collection.Sequence[A]
                     with GenericTraversableTemplate[A, Sequence]
                     with SequenceLike[A, Sequence[A]] {
  override def companion: GenericCompanion[Sequence] = Sequence

  def update(idx: Int, elem: A)
}

/** A factory object for the trait <code>Sequence</code>.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
object Sequence extends SequenceFactory[Sequence] {
  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Sequence[A]] = new ArrayBuffer
}
