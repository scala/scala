/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._

/** A subtrait of `collection.IndexedSeq` which represents sequences
 *  that can be mutated.
 *
 *  $indexedSeqInfo
 */
trait IndexedSeq[A] extends Seq[A]
                   with scala.collection.IndexedSeq[A]
                   with GenericTraversableTemplate[A, IndexedSeq]
                   with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq]  = IndexedSeq
  override def seq: IndexedSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable indexed sequence
 *  @define Coll `mutable.IndexedSeq`
 */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer[A]
}
