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
import collection.mutable.{Vector, ArrayBuffer}
import collection.generic._

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 */
abstract class BoxedArray[A] extends Vector[A] with VectorTemplate[A, BoxedArray[A]] with Boxed {

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): A

  /** Update element at given index */
  def update(index: Int, elem: A): Unit

  /** Creates new builder for this collection ==> move to subclasses
   *
   * */
  override protected[this] def newBuilder = genericBuilder[A]

  override def genericBuilder[B]: Builder[B, BoxedArray[B]] = new ArrayBuffer[B].mapResult {
    _.toArray.asInstanceOf[BoxedArray[B]]
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

  final def deepToString() = deepMkString(stringPrefix + "(", ", ", ")")

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

  final def deepMkString(sep: String): String = this.deepMkString("", sep, "")

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

  protected def countAndMemo(p: A => Boolean): (Int, Array[Boolean]) = {
    val len = length
    val memo = new Array[Boolean](len)
    var count = 0
    var i = 0
    while (i < len) {
      if (p(this(i))) { memo(i) = true; count += 1 }
      i += 1
    }
    (count, memo)
  }

  /** @deprecated use slice instead */
  @deprecated def subArray(from: Int, end: Int): BoxedArray[A] = slice(from, end)
}
