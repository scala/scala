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

/** A common supertrait of `ArrayOps` and `WrappedArray` that factors out the 
 * `deep` method for arrays and wrapped arrays and serves as a marker trait
 * for array wrappers.
 *
 *  @tparam A     type of the elements contained in the array like object.
 *  @tparam Repr  the type of the actual collection containing the elements.
 *
 *  @define Coll `ArrayLike`
 *  @version 2.8
 *  @since   2.8
 */
trait ArrayLike[A, +Repr] extends Any with IndexedSeqOptimized[A, Repr] { self =>

  /** Creates a possible nested `IndexedSeq` which consists of all the elements
   *  of this array. If the elements are arrays themselves, the `deep` transformation
   *  is applied recursively to them. The `stringPrefix` of the `IndexedSeq` is
   *  "Array", hence the `IndexedSeq` prints like an array with all its
   *  elements shown, and the same recursively for any subarrays.
   *
   *  Example:
   *  {{{
   *  Array(Array(1, 2), Array(3, 4)).deep.toString
   *  }}}
   *  prints: `Array(Array(1, 2), Array(3, 4))`
   *
   *  @return    An possibly nested indexed sequence of consisting of all the elements of the array.
   */
  def deep: scala.collection.IndexedSeq[Any] = new scala.collection.AbstractSeq[Any] with scala.collection.IndexedSeq[Any] {
    def length = self.length
    def apply(idx: Int): Any = self.apply(idx) match {
      case x: AnyRef if x.getClass.isArray => WrappedArray.make(x).deep
      case x => x
    }
    override def stringPrefix = "Array"
  }
}
