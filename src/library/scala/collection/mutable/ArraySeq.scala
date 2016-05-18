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

import generic._
import parallel.mutable.ParArray

/** A class for polymorphic arrays of elements that's represented
 *  internally by an array of objects. This means that elements of
 *  primitive types are boxed.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#array_sequences "Scala's Collection Library overview"]]
 *  section on `Array Sequences` for more information.
 *
 *  @tparam A      type of the elements contained in this array sequence.
 *  @param length  the length of the underlying array.
 *
 *  @define Coll `ArraySeq`
 *  @define coll array sequence
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ArraySeq[B]` because an implicit of type `CanBuildFrom[ArraySeq, B, ArraySeq[B]]`
 *    is defined in object `ArraySeq`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ArraySeq`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(1530165946227428979L)
class ArraySeq[A](override val length: Int)
extends AbstractSeq[A]
   with IndexedSeq[A]
   with GenericTraversableTemplate[A, ArraySeq]
   with IndexedSeqOptimized[A, ArraySeq[A]]
   with CustomParallelizable[A, ParArray[A]]
   with Serializable
{

  override def companion: GenericCompanion[ArraySeq] = ArraySeq

  val array: Array[AnyRef] = new Array[AnyRef](length)

  override def par = ParArray.handoff(array.asInstanceOf[Array[A]], length)

  def apply(idx: Int): A = {
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) {
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  override def foreach[U](f: A => U) {
    var i = 0
    while (i < length) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  /** Fills the given array `xs` with at most `len` elements of
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

  override def clone(): ArraySeq[A] = {
    val cloned = array.clone().asInstanceOf[Array[AnyRef]]
    new ArraySeq[A](length) {
      override val array = cloned
    }
  }

}

/** $factoryInfo
 *  @define coll array sequence
 *  @define Coll `ArraySeq`
 */
object ArraySeq extends SeqFactory[ArraySeq] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ArraySeq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, ArraySeq[A]] =
    new ArrayBuffer[A] mapResult { buf =>
      val result = new ArraySeq[A](buf.length)
      buf.copyToArray(result.array.asInstanceOf[Array[Any]], 0)
      result
    }
}
