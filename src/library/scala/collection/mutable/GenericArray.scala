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

/** A class for polymorphic arrays of elements that's represented
 *  internally by an array of objects. This means that elements of
 *  primitive types are boxed.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
class GenericArray[A](override val length: Int)
extends IndexedSeq[A]
   with GenericTraversableTemplate[A, GenericArray]
   with IndexedSeqLike[A, GenericArray[A]] {

  override def companion: GenericCompanion[GenericArray] = GenericArray

  val array: Array[AnyRef] = new Array[AnyRef](length)

  def apply(idx: Int): A = {
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) {
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  override def foreach[U](f: A =>  U) {
    var i = 0
    while (i < length) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this traversable starting at position `start`.
   *  Copying will stop once either the end of the current traversable is reached or
   *  `len` elements have been copied or the end of the array is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
   override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
     val len1 = len min (xs.length - start) min length
     Array.copy(array, 0, xs, start, len1)
   }
}

object GenericArray extends SeqFactory[GenericArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, GenericArray[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, GenericArray[A]] =
    new ArrayBuffer[A] mapResult { buf =>
      val result = new GenericArray[A](buf.length)
      buf.copyToArray(result.array.asInstanceOf[Array[Any]], 0)
      result
    }
}
