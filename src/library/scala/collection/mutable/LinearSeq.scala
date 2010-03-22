/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A subtrait of <code>collection.LinearSeq</code> which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 */
trait LinearSeq[A] extends Seq[A]
                           with scala.collection.LinearSeq[A]
                           with GenericTraversableTemplate[A, LinearSeq]
                           with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
}

object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = new MutableList[A]
}
