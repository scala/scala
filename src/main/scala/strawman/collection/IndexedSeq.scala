package strawman
package collection

import scala.{Any, Int}

/** Base trait for indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A] with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends Any with SeqOps[A, CC, C] { self =>

  def iterator(): Iterator[A] = view.iterator()

  override def view: IndexedView[A] = new IndexedView[A] {
    def length: Int = self.length
    def apply(i: Int): A = self(i)
  }

  override protected[this] def reversed: Iterable[A] = view.reverse

  /** A collection containing the last `n` elements of this collection. */
  override def takeRight(n: Int): C = fromSpecificIterable(view.takeRight(n))

  /** The rest of the collection without its `n` last elements. For
    * linear, immutable collections this should avoid making a copy. */
  override def dropRight(n: Int): C = fromSpecificIterable(view.dropRight(n))

}
