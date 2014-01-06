/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package immutable

import generic._
import mutable.{ArrayBuffer, Builder}

/** A subtrait of `collection.IndexedSeq` which represents indexed sequences
 *  that are guaranteed immutable.
 *  $indexedSeqInfo
 */
trait IndexedSeq[+A] extends Seq[A]
                    with scala.collection.IndexedSeq[A]
                    with GenericTraversableTemplate[A, IndexedSeq]
                    with IndexedSeqLike[A, IndexedSeq[A]] {
  override def companion: GenericCompanion[IndexedSeq] = IndexedSeq
  
  /** Returns this $coll as an indexed sequence.
   *  
   *  A new indexed sequence will not be built; lazy collections will stay lazy.
   */
  @deprecatedOverriding("Immutable indexed sequences should do nothing on toIndexedSeq except cast themselves as an indexed sequence.", "2.11.0")
  override def toIndexedSeq: IndexedSeq[A] = this
  override def seq: IndexedSeq[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `Vector`.
 *  @define coll indexed sequence
 *  @define Coll `IndexedSeq`
 */
object IndexedSeq extends IndexedSeqFactory[IndexedSeq] {
  class Impl[A](buf: ArrayBuffer[A]) extends AbstractSeq[A] with IndexedSeq[A] with Serializable {
    def length = buf.length
    def apply(idx: Int) = buf.apply(idx)
  }
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = Vector.newBuilder[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
}
