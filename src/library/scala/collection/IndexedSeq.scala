/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._
import mutable.Builder

/** A base trait for indexed sequences.
 *  $indexedSeqInfo
 */
trait IndexedSeq[+A] extends Seq[A]
                    with GenericTraversableTemplate[A, IndexedSeq]
                    with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq] = IndexedSeq
  override def seq: IndexedSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `Vector`.
 *  @define coll indexed sequence
 *  @define Coll IndexedSeq
 */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = immutable.IndexedSeq.newBuilder[A]
}

