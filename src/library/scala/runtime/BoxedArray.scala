/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._
import collection.mutable.ArrayBuffer

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 */
abstract class BoxedArray extends Array.Array0[Any] {
  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): Any

  /** Update element at given index */
  def update(index: Int, elem: Any): Unit

  /** Convert to Java array.
   *  @param elemTag    Either one of the tags ".N" where N is the name of a primitive type
   *                    (@see ScalaRunTime), or a full class name.
   */
  //todo: remove
  def unbox(elemTag: String): AnyRef

  def unbox(elemClass: Class[_]): AnyRef

  override def isDefinedAt(x: Int): Boolean = 0 <= x && x < length

  override def elements = new Iterator[Any] {
    var index = 0
    def hasNext: Boolean = index < length
    def next(): Any = { val i = index; index = i + 1; apply(i) }
  }

  /** The underlying array value
   */
  def value: AnyRef

  def copyFrom(src: AnyRef, from: Int, to: Int, len: Int): Unit =
    Array.copy(src, from, value, to, len)

  def copyTo(from: Int, dest: AnyRef, to: Int, len: Int): Unit = {
    Array.copy(value, from, dest, to, len)
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  override def copyToArray[B](xs: Array[B], start: Int): Unit =
    copyTo(0, xs, start, length)

  // todo: add a copyToBuffer

  // todo: eliminate
  def subArray(from: Int, end: Int): AnyRef

  final override def map[b](f: Any => b): Array[b] = {
    val len = length
    val result = new Array[b](len)
    var i = 0
    while (i < len) {
      result(i) = f(apply(i))
      i += 1
    }
    result
  }

  final override def flatMap[b](f: Any => Iterable[b]): Array[b] = {
    val buf = new ArrayBuffer[b]
    val len = length
    var i = 0
    while (i < len) {
      buf ++= f(apply(i))
      i += 1
    }
    buf.toArray
  }

  final override def ++[b >: Any](that: Iterable[b]): Array[b] = super.++(that).toArray

  final def zip[b](that: Array[b]): Array[Tuple2[Any,b]] = {
    val len = length
    if(len != that.length)
      throw new Error("zipping arrays of different length")
    val result = new Array[Tuple2[Any,b]](len)
    var i = 0
    while (i < len) {
      result(i) = new Tuple2(this(i), that(i))
      i += 1
    }
    result
  }

  final def zipWithIndex: Array[Tuple2[Any,Int]] = {
    val len = length
    val result = new Array[Tuple2[Any,Int]](len)
    var i = 0
    while (i < len) {
      result(i) = new Tuple2(this(i), i)
      i += 1
    }
    result
  }

  /** Returns an array that contains all indices of this array */
  def indices: Array[Int] = Array.range(0, length)

  override def takeWhile(p: Any => Boolean) = {
    val c = length + 1
    take((findIndexOf(!p(_)) + c) % c)
  }
  override def dropWhile(p: Any => Boolean) = {
    val c = length + 1
    drop((findIndexOf(!p(_)) + c) % c)
  }

  final def deepToString() = deepMkString(stringPrefix + "(", ",", ")")

  final def deepMkString(start: String, sep: String, end: String): String = {
    def _deepToString(x: Any) = x match {
      case a: AnyRef if ScalaRunTime.isArray(a) =>
        ScalaRunTime.boxArray(a).deepMkString(start, sep, end)
      case _ =>
        x.toString
    }
    val buf = new StringBuilder()
    buf.append(start)
    val elems = elements
    if (elems.hasNext) buf.append(_deepToString(elems.next))
    while (elems.hasNext) {
      buf.append(sep); buf.append(_deepToString(elems.next))
    }
    buf.append(end)
    buf.toString
  }

  final def deepMkString(sep: String): String = this.deepMkString("", sep, "")

  final def deepEquals(that: Any): Boolean = {
    def _deepEquals(x1: Any, x2: Any) = (x1, x2) match {
      case (a1: BoxedArray, a2: BoxedArray) =>
        _sameElements(a1, a2)
      case (a1: AnyRef, a2: AnyRef)
           if ScalaRunTime.isArray(a1) && ScalaRunTime.isArray(a2) =>
        _sameElements(ScalaRunTime.boxArray(a1), ScalaRunTime.boxArray(a2))
      case _ =>
        x1.equals(x2)
    }
    def _sameElements(a1: BoxedArray, a2: BoxedArray): Boolean = {
      val it1 = a1.elements
      val it2 = a2.elements
      var res = true
      while (res && it1.hasNext && it2.hasNext)
        res = _deepEquals(it1.next, it2.next)
      !it1.hasNext && !it2.hasNext && res
    }
    that match {
      case a: BoxedArray =>
        _sameElements(this, a)
      case a: AnyRef if ScalaRunTime.isArray(a) =>
        _sameElements(this, ScalaRunTime.boxArray(a))
      case _ =>
        false
    }
  }
  override final def stringPrefix: String = "Array"

  protected def newArray(length : Int, elements : Iterator[Any]) : BoxedArray
  override def projection : scala.Array.Projection[Any] = new scala.Array.Projection[Any] {
    def update(idx : Int, what : Any) : Unit = BoxedArray.this.update(idx, what)
    def length = BoxedArray.this.length
    def apply(idx : Int) = BoxedArray.this.apply(idx)
    override def stringPrefix = "ArrayP"
    protected def newArray[B >: Any](length : Int, elements : Iterator[Any]) =
      BoxedArray.this.newArray(length, elements).asInstanceOf[Array[B]]
  }
}
