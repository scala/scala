/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.Builder

/** A subtrait of collection.Sequence which represents sequences
 *  that cannot be mutated.
 *
 *  @since 2.8
 */
trait Sequence[+A] extends Iterable[A]
                      with scala.collection.Sequence[A]
                      with GenericTraversableTemplate[A, Sequence]
                      with SequenceLike[A, Sequence[A]] {
  override def companion: GenericCompanion[Sequence] = Sequence
}

/**
 * @since 2.8
 */
object Sequence extends SequenceFactory[Sequence] {
  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Sequence[A]] = new mutable.ListBuffer
}
