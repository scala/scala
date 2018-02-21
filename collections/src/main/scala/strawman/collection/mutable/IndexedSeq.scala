package strawman.collection
package mutable

import scala.AnyRef

trait IndexedSeq[T] extends Seq[T]
  with strawman.collection.IndexedSeq[T]
  with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]] {

  override def iterableFactory: SeqFactory[IterableCC] = IndexedSeq
}

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](ArrayBuffer)

trait IndexedSeqOps[A, +CC[_], +C <: AnyRef]
  extends strawman.collection.IndexedSeqOps[A, CC, C]
  with SeqOps[A, CC, C]
