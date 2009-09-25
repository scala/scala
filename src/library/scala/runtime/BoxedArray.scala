/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import Predef._
import scala.reflect.ClassManifest
import collection.mutable._
import collection.Sequence

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 */
abstract class BoxedArray[A] extends Vector[A] with VectorLike[A, BoxedArray[A]] with Boxed { self =>

  val ex = new Error("trying to create a BoxedArray")
  ex.printStackTrace()
  throw ex

  /** The manifest of the element type */
  def elemManifest: ClassManifest[A]

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): A

  /** Update element at given index */
  def update(index: Int, elem: A): Unit

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[A, BoxedArray[A]] =
    genericBuilder[A]

  // !!! todo: remove
  override def genericBuilder[B]: Builder[B, BoxedArray[B]] = new ArrayBuffer[B].mapResult {
    _.toArray(null).asInstanceOf[BoxedArray[B]]
  }

  /** Convert to Java array.
   *  @param elemTag    Either one of the tags ".N" where N is the name of a primitive type
   *                    (@see ScalaRunTime), or a full class name.
   */
  def unbox(elemClass: Class[_]): AnyRef

  /** The underlying array value
   */
  def value: AnyRef

  def copyFrom(src: AnyRef, from: Int, to: Int, len: Int): Unit =
    Array.copy(src, from, value, to, len)

  def copyTo(from: Int, dest: AnyRef, to: Int, len: Int): Unit = {
    Array.copy(value, from, dest, to, len)
  }

  override def toArray[B >: A](implicit m: ClassManifest[B]): Array[B] = {
    if ((elemManifest ne null) && (elemManifest.erasure eq m.erasure)) this.asInstanceOf[Array[B]]
    else super.toArray[B]
  }

/*
  override def equals(other: Any) =
    (value eq other) ||

    other.isInstanceOf[BoxedArray[_]] && (value == other.asInstanceOf[BoxedArray[_]].value)

  override def hashCode(): Int = value.hashCode()
*/
  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  override def copyToArray[B](xs: Array[B], start: Int, len: Int): Unit =
    copyTo(0, xs, start, len)

  /** Creates a possible nested vector which consists of all the elements
   *  of this array. If the elements are arrays themselves, the `deep' transformation
   *  is applied recursively to them. The stringPrefix of the vector is
   *  "Array", hence the vector prints like an array with all its
   *  elements shown, and the same recursively for any subarrays.
   *
   *  Example:   Array(Array(1, 2), Array(3, 4)).deep.toString
   *  prints:    Array(Array(1, 2), Array(3, 4))
   */
  def deep: collection.Vector[Any] = new collection.Vector[Any] {
    def length = self.length
    def apply(idx: Int): Any = self.apply(idx) match {
      case elem: AnyRef if ScalaRunTime.isArray(elem) => ScalaRunTime.boxArray(elem).deep
      case elem => elem
    }
    override def stringPrefix = "Array"
  }

  @deprecated("use deep.toString instead")
  final def deepToString() = deepMkString(stringPrefix + "(", ", ", ")")

  @deprecated("use deep.mkString instead")
  final def deepMkString(start: String, sep: String, end: String): String = {
    def _deepToString(x: Any) = x match {
      case a: AnyRef if ScalaRunTime.isArray(a) =>
        ScalaRunTime.boxArray(a).deepMkString(start, sep, end)
      case _ =>
        ScalaRunTime.stringOf(x)
    }
    val buf = new StringBuilder()
    buf.append(start)
    val iter = this.iterator
    if (iter.hasNext) buf.append(_deepToString(iter.next))
    while (iter.hasNext) {
      buf.append(sep); buf.append(_deepToString(iter.next))
    }
    buf.append(end)
    buf.toString
  }

  @deprecated("use deep.mkString instead")
  final def deepMkString(sep: String): String = this.deepMkString("", sep, "")

  @deprecated("use array1.deep.equals(array2.deep) instead")
  final def deepEquals(that: Any): Boolean = {
    def _deepEquals(x1: Any, x2: Any) = (x1, x2) match {
      case (a1: BoxedArray[_], a2: BoxedArray[_]) =>
        _sameElements(a1, a2)
      case (a1: AnyRef, a2: AnyRef)
           if ScalaRunTime.isArray(a1) && ScalaRunTime.isArray(a2) =>
        _sameElements(ScalaRunTime.boxArray(a1), ScalaRunTime.boxArray(a2))
      case _ =>
        x1.equals(x2)
    }
    def _sameElements(a1: BoxedArray[_], a2: BoxedArray[_]): Boolean = {
      val it1 = a1.iterator
      val it2 = a2.iterator
      var res = true
      while (res && it1.hasNext && it2.hasNext)
        res = _deepEquals(it1.next, it2.next)
      !it1.hasNext && !it2.hasNext && res
    }
    that match {
      case a: BoxedArray[_] =>
        _sameElements(this, a)
      case a: AnyRef if ScalaRunTime.isArray(a) =>
        _sameElements(this, ScalaRunTime.boxArray(a))
      case _ =>
        false
    }
  }

  override final def stringPrefix: String = "Array"
}
