/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable
import generic._

/** A subtrait of collection.Vector which represents sequences
 *  that can be mutated.
 */
trait ArrayLike[A, +Repr] extends VectorLike[A, Repr] { self =>

  /** Creates a possible nested vector which consists of all the elements
   *  of this array. If the elements are arrays themselves, the `deep' transformation
   *  is applied recursively to them. The stringPrefix of the vector is
   *  "Array", hence the vector prints like an array with all its
   *  elements shown, and the same recursively for any subarrays.
   *
   *  Example:   Array(Array(1, 2), Array(3, 4)).deep.toString
   *  prints:    Array(Array(1, 2), Array(3, 4))
   */
  def deep: scala.collection.Vector[Any] = new scala.collection.Vector[Any] {
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
