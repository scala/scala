/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IndexedSeq.scala 19035 2009-10-10 22:54:28Z rompf $


package scala.collection

import generic._
import scala.collection.mutable.Builder

/** <p>
 *    Sequences that support O(1) element access and O(1) length computation.
 *  </p>
 *  <p>
 *    This class does not add any methods to <code>Sequence</code> but
 *    overrides several methods with optimized implementations.
 *  </p>
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait IndexedSeq[+A] extends Seq[A]
                    with GenericTraversableTemplate[A, IndexedSeq]
                    with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq] = IndexedSeq
}

object IndexedSeq extends SeqFactory[IndexedSeq] {
  override def empty[A]: IndexedSeq[A] = immutable.IndexedSeq.empty[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = immutable.IndexedSeq.newBuilder[A]

  @deprecated("use collection.mutable.IndexedSeq instead") type Mutable[A] = scala.collection.mutable.IndexedSeq[A]
}
