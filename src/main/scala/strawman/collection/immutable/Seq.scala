package strawman
package collection
package immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.Any

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]

trait SeqOps[+A, +CC[A] <: Seq[A], +C] extends collection.SeqOps[A, CC, C]

object Seq extends IterableFactory.Delegate[Seq](List)

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

/** Base trait for immutable indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends SeqOps[A, CC, C] with collection.IndexedSeqOps[A, CC, C]
