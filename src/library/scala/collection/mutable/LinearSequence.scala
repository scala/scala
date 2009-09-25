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
 *  that cannot be mutated.
 */
trait LinearSequence[A] extends Sequence[A]
                           with scala.collection.LinearSequence[A]
                           with GenericTraversableTemplate[A, LinearSequence]
                           with LinearSequenceLike[A, LinearSequence[A]] {
  override def companion: GenericCompanion[LinearSequence] = LinearSequence
}

object LinearSequence extends SequenceFactory[LinearSequence] {
  implicit def builderFactory[A]: BuilderFactory[A, LinearSequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, LinearSequence[A]] = new MutableList[A]
}
