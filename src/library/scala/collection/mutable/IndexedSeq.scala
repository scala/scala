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

/** A subtrait of <code>collection.IndexedSeq</code> which represents sequences
 *  that can be mutated.
 *  $indexedSeqInfo
 */
trait IndexedSeq[A] extends Seq[A]
                   with scala.collection.IndexedSeq[A]
                   with GenericTraversableTemplate[A, IndexedSeq]
                   with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq]  = IndexedSeq
}

/** $factoryInfo
 *  @define coll indexed sequence
 *  @define Coll IndexedSeq
 */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer[A]
}
