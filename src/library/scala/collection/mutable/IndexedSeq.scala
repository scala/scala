package scala.collection
package mutable

import scala.language.higherKinds

trait IndexedSeq[T] extends Seq[T]
  with scala.collection.IndexedSeq[T]
  with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]] {

  override def iterableFactory: SeqFactory[IterableCC] = IndexedSeq
}

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](ArrayBuffer)

trait IndexedSeqOps[A, +CC[_], +C <: AnyRef]
  extends scala.collection.IndexedSeqOps[A, CC, C]
  with SeqOps[A, CC, C]
