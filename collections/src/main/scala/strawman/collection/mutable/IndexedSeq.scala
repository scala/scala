package strawman.collection
package mutable

trait IndexedSeq[T] extends Seq[T]
  with strawman.collection.IndexedSeq[T]
  with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]]

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](ArrayBuffer)

trait IndexedSeqOps[A, +CC[X] <: IndexedSeq[X], +C <: IndexedSeq[A]]
  extends strawman.collection.IndexedSeqOps[A, CC, C]
  with SeqOps[A, CC, C]
