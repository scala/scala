/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.generic

/** A subtrait of collection.Vector which represents sequences
 *  that can be mutated.
 */
trait MutableVectorTemplate[A, +This <: MutableVectorTemplate[A, This] with mutable.Vector[A]] extends VectorTemplate[A, This] { self =>

  def update(idx: Int, elem: A)

  /** Creates a view of this iterable @see Iterable.View
   */
  override def view = new MutableVectorView[A, This] {
    protected lazy val underlying = self.thisCollection
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
