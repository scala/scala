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

/** A subtrait of `collection.LinearSeq` which represents sequences
 *  that can be mutated.
 *  $linearSeqInfo
 *
 *  @define Coll LinearSeq
 *  @define coll linear sequence
 */
trait LinearSeq[A] extends Seq[A]
                           with scala.collection.LinearSeq[A]
                           with GenericTraversableTemplate[A, LinearSeq]
                           with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `MutableList`.
 *  @define coll mutable linear sequence
 *  @define Coll mutable.LinearSeq
 */
object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = new MutableList[A]
}
