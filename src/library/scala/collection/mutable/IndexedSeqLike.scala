/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  It declares a method `update` which allows updating an element
 *  at a specific index in the sequence.
 *
 *  This trait just implements `iterator` in terms of `apply` and `length`.
 *  However, see `IndexedSeqOptimized` for an implementation trait that overrides operations
 *  to make them run faster under the assumption of fast random access with `apply`.
 *
 *  $indexedSeqInfo
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 *
 *  @define Coll `IndexedSeq`
 *  @define coll mutable indexed sequence
 *  @define indexedSeqInfo
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait IndexedSeqLike[A, +Repr] extends Any with scala.collection.IndexedSeqLike[A, Repr] { self =>

  override protected[this] def thisCollection: IndexedSeq[A] = this.asInstanceOf[IndexedSeq[A]]
  override protected[this] def toCollection(repr: Repr): IndexedSeq[A] = repr.asInstanceOf[IndexedSeq[A]]

  /** Replaces element at given index with a new value.
   *
   *  @param idx     the index of the element to replace.
   *  @param elem    the new value.
   *  @throws   IndexOutOfBoundsException if the index is not valid.
   */
  def update(idx: Int, elem: A)

  /** Creates a view of this iterable @see Iterable.View
   */
  override def view = new IndexedSeqView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
    override def update(idx: Int, elem: A) = self.update(idx, elem)
  }

  /** A sub-sequence view  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `view` and `slice` is that `view` produces
   *         a view of the current sequence, whereas `slice` produces a new sequence.
   *
   *  @note view(from, to)  is equivalent to view.slice(from, to)
   */
  override def view(from: Int, until: Int) = view.slice(from, until)
}
