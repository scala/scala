/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

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
 *  Indexed sequences do not add any new methods to `Seq`, but promise
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
trait IndexedSeqLike[+A, +Repr] extends Any with SeqLike[A, Repr] {
  self =>

  def seq: IndexedSeq[A]
  override def hashCode()= scala.util.hashing.MurmurHash3.seqHash(seq)  // TODO - can we get faster via "indexedSeqHash" ?

  override protected[this] def thisCollection: IndexedSeq[A] = this.asInstanceOf[IndexedSeq[A]]
  override protected[this] def toCollection(repr: Repr): IndexedSeq[A] = repr.asInstanceOf[IndexedSeq[A]]

  /** The class of the iterator returned by the `iterator` method.
   *  multiple `take`, `drop`, and `slice` operations on this iterator are bunched
   *  together for better efficiency.
   */
  // pre: start >= 0, end <= self.length
  @SerialVersionUID(1756321872811029277L)
  protected class Elements(start: Int, end: Int) extends AbstractIterator[A] with BufferedIterator[A] with Serializable {
    private var index = start
    private def available = (end - index) max 0

    def hasNext: Boolean = index < end

    def next(): A = {
      if (index >= end)
        Iterator.empty.next()

      val x = self(index)
      index += 1
      x
    }

    def head = {
      if (index >= end)
        Iterator.empty.next()

      self(index)
    }

    override def drop(n: Int): Iterator[A] =
      if (n <= 0) new Elements(index, end)
      else if (index + n >= end) new Elements(end, end)
      else new Elements(index + n, end)
    override def take(n: Int): Iterator[A] =
      if (n <= 0) Iterator.empty
      else if (n <= available) new Elements(index, index + n)
      else new Elements(index, end)
    override def slice(from: Int, until: Int): Iterator[A] =
      this take until drop from
  }

  override /*IterableLike*/
  def iterator: Iterator[A] = new Elements(0, length)

  /* Overridden for efficiency */
  override def toBuffer[A1 >: A]: mutable.Buffer[A1] = {
    val result = new mutable.ArrayBuffer[A1](size)
    copyToBuffer(result)
    result
  }

  override protected[collection] def sizeHintIfCheap: Int = size
}
