/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef.{Class, Error}
import collection.mutable.ArrayBuffer

/**
 *  <p>A class representing <code>Array[T]</code></p>
 */
abstract class BoxedArray extends Seq[Any] {
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

  def unbox(elemClass: Class): AnyRef

  override def isDefinedAt(x: Int): Boolean = 0 <= x && x < length

  def elements = new Iterator[Any] {
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

  final def deepToString() = deepMkString(stringPrefix + "(", ",", ")")

  final def deepMkString(start: String, sep: String, end: String): String = {
    val buf = new StringBuilder()
    deepAddString(buf, start, sep, end).toString
  }

  final def deepMkString(sep: String): String = this.deepMkString("", sep, "")

  private def deepAddString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    def _deepToString(x: Any) = x match {
      case a: AnyRef if ScalaRunTime.isArray(a) =>
        ScalaRunTime.boxArray(a).deepMkString(start, sep, end)
      case _ =>
        x.toString
    }
    buf.append(start)
    val elems = elements
    if (elems.hasNext) buf.append(_deepToString(elems.next))
    while (elems.hasNext) {
      buf.append(sep); buf.append(_deepToString(elems.next))
    }
    buf.append(end)
  }

  override final def stringPrefix: String = "Array"
}
