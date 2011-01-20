/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._
import mutable.ArrayBuffer
import scala.annotation.tailrec

/** A template trait for indexed sequences of type `IndexedSeq[A]`.
 *
 *  $indexedSeqInfo
 *
 *  This trait just implements `iterator` in terms of `apply` and `length`.
 *  However, see `IndexedSeqOptimized` for an implementation trait that overrides operations
 *  to make them run faster under the assumption of fast random access with `apply`.
 *
 *  @define  Coll  IndexedSeq
 *  @define indexedSeqInfo
 *  Indexed sequences support constant-time or near constant-time element
 *  access and length computation. They are defined in terms of abstract methods
 *  `apply` for indexing and `length`.
 *
 *  Indexed sequences do not add any new methods wrt `Seq`, but promise
 *  efficient implementations of random access patterns.
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait IndexedSeqLike[+A, +Repr] extends SeqLike[A, Repr] { self =>

  override protected[this] def thisCollection: IndexedSeq[A] = this.asInstanceOf[IndexedSeq[A]]
  override protected[this] def toCollection(repr: Repr): IndexedSeq[A] = repr.asInstanceOf[IndexedSeq[A]]

  /** The class of the iterator returned by the `iterator` method.
   *  multiple `take`, `drop`, and `slice` operations on this iterator are bunched
   *  together for better efficiency.
   */
  @SerialVersionUID(1756321872811029277L)
  protected class Elements(start: Int, end: Int) extends BufferedIterator[A] with Serializable {
    private var i = start

    def hasNext: Boolean = i < end

    def next: A =
      if (i < end) {
        val x = self(i)
        i += 1
        x
      } else Iterator.empty.next

    def head =
      if (i < end) self(i) else Iterator.empty.next

    /** $super
     *  '''Note:''' `drop` is overridden to enable fast searching in the middle of indexed sequences.
     */
    override def drop(n: Int): Iterator[A] =
      if (n > 0) new Elements(i + n, end) else this

    /** $super
     *  '''Note:''' `take` is overridden to be symmetric to `drop`.
     */
    override def take(n: Int): Iterator[A] =
      if (n <= 0) Iterator.empty.buffered
      else if (i + n < end) new Elements(i, i + n)
      else this
  }

  override /*IterableLike*/
  def iterator: Iterator[A] = new Elements(0, length)

  /** Overridden for efficiency */
  override def toBuffer[A1 >: A]: mutable.Buffer[A1] = {
    val result = new mutable.ArrayBuffer[A1](size)
    copyToBuffer(result)
    result
  }


/*
  override /*SeqLike*/
  def view = new IndexedSeqView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override /*SeqLike*/
  def view(from: Int, until: Int) = view.slice(from, until)
*/
}

