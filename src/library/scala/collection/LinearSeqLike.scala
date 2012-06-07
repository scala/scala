/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection

import generic._
import mutable.ListBuffer
import immutable.List
import scala.util.control.Breaks._
import annotation.tailrec

/** A template trait for linear sequences of type `LinearSeq[A]`.
 *
 *  $linearSeqInfo
 *
 *  This trait just implements `iterator` in terms of `isEmpty, ``head`, and `tail`.
 *  However, see `LinearSeqOptimized` for an implementation trait that overrides operations
 *  to make them run faster under the assumption of fast linear access with `head` and `tail`.
 *
 *  @define  linearSeqInfo
 *  Linear sequences are defined in terms of three abstract methods, which are assumed
 *  to have efficient implementations. These are:
 *  {{{
 *     def isEmpty: Boolean
 *     def head: A
 *     def tail: Repr
 *  }}}
 *  Here, `A` is the type of the sequence elements and `Repr` is the type of the sequence itself.
 *
 *  Linear sequences do not add any new methods to `Seq`, but promise efficient implementations
 *  of linear access patterns.
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 */
trait LinearSeqLike[+A, +Repr <: LinearSeqLike[A, Repr]] extends SeqLike[A, Repr] {
  self: Repr =>

  override protected[this] def thisCollection: LinearSeq[A] = this.asInstanceOf[LinearSeq[A]]
  override protected[this] def toCollection(repr: Repr): LinearSeq[A] = repr.asInstanceOf[LinearSeq[A]]

  def seq: LinearSeq[A]

  override def hashCode() = util.hashing.MurmurHash3.seqHash(seq) // TODO - can we get faster via "linearSeqHash" ?

  override /*IterableLike*/
  def iterator: Iterator[A] = new AbstractIterator[A] {
    var these = self
    def hasNext: Boolean = !these.isEmpty
    def next(): A =
      if (hasNext) {
        val result = these.head; these = these.tail; result
      } else Iterator.empty.next

    /** Have to clear `these` so the iterator is exhausted like
     *  it would be without the optimization.
     */
    override def toList: List[A] = {
      val xs = these.toList
      these = newBuilder.result
      xs
    }
  }

  @tailrec override final def corresponds[B](that: GenSeq[B])(p: (A,B) => Boolean): Boolean = {
    if (this.isEmpty) that.isEmpty
    else that.nonEmpty && p(head, that.head) && (tail corresponds that.tail)(p)
  }
}
