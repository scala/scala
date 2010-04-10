/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable
import generic._

/** A subtrait of `collection.IndexedSeq` which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 *
 *  @tparam A     type of the elements contained in the array like object.
 *  @tparam Repr  the type of the actual collection containing the elements.
 *
 *  @define Coll ArrayLike
 */
trait ArrayLike[A, +Repr] extends IndexedSeqOptimized[A, Repr] { self =>

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
  def deep: scala.collection.IndexedSeq[Any] = new scala.collection.IndexedSeq[Any] {
    def length = self.length
    def apply(idx: Int): Any = self.apply(idx) match {
      case x: AnyRef if x.getClass.isArray => WrappedArray.make(x).deep
      case x => x
    }
    override def stringPrefix = "Array"
  }

  @deprecated("use deep.toString instead")
  final def deepToString() =
    deep.toString

  @deprecated("use deep.mkString instead")
  final def deepMkString(start: String, sep: String, end: String): String =
    deep.mkString(start, sep, end)

  @deprecated("use deep.mkString instead")
  final def deepMkString(sep: String): String =
    deepMkString("", sep, "")

  @deprecated("use array1.deep.equals(array2.deep) instead")
  final def deepEquals(that: Any): Boolean = that match {
    case x: AnyRef if x.getClass.isArray => deep.equals(WrappedArray.make(x).deep)
    case _ => false
  }
}
