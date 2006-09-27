/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *  <dl>
 *    <dt><b>Direct Known Subclasses:</b></dt>
 *    <dd>
 *      <a href="BoxedAnyArray.html" target="contentFrame">BoxedAnyArray</a>,
 *      <a href="BoxedBooleanArray.html" target="contentFrame">BoxedBooleanArray</a>,
 *      <a href="BoxedByteArray.html" target="contentFrame">BoxedByteArray</a>,
 *      <a href="BoxedDoubleArray.html" target="contentFrame">BoxedDoubleArray</a>,
 *      <a href="BoxedFloatArray.html" target="contentFrame">BoxedFloatArray</a>,
 *      <a href="BoxedIntArray.html" target="contentFrame">BoxedIntArray</a>,
 *      <a href="BoxedLongArray.html" target="contentFrame">BoxedLongArray</a>,
 *      <a href="BoxedObjectArray.html" target="contentFrame">BoxedObjectArray</a>,
 *      <a href="BoxedShortArray.html" target="contentFrame">BoxedShortArray</a>
 *   </dd>
 *  </dl>
 */
abstract class BoxedArray extends PartialFunction[Int, Object] with Seq[Object] {
  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): Object

  /** Update element at given index */
  def update(index: Int, elem: Object): Unit

  /** Convert to Java array.
   *  @param elemTag    Either one of the tags ".N" where N is the name of a primitive type
   *                    (@see ScalaRunTime), or a full class name.
   */
  def unbox(elemTag: String): Object

  def unbox(elemClass: Class): Object

  override def isDefinedAt(x: Int): Boolean   =  0 <= x && x < length

  def elements = new Iterator[Object] {
    var index = 0
    def hasNext: Boolean = index < length
    def next: Object = { val i = index; index = i + 1; apply(i) }
  }

  /** The underlying array value
   */
  def value: Object

  def copyFrom(src: Object, from: Int, to: Int, len: Int): Unit =
    Array.copy(src, from, value, to, len)

  def copyTo(from: Int, dest: Object, to: Int, len: Int): Unit = {
    Array.copy(value, from, dest, to, len)
  }

  override def toArray[b>:Object]: Array[b] = {
    val len = length
    val res = new Array[b](len)
    copyTo(0, res, 0, len)
    res
  }

  def subArray(from: Int, end: Int): Object

  def filter(p: Any => Boolean): Object

  final def map[b](f: Any => b): Array[b] = {
    val len = length
    val result = new Array[b](len)
    var i = 0
    while (i < len) {
      result(i) = f(apply(i))
      i = i + 1
    }
    result
  }

  final def flatMap[b](f: Any => Array[b]): Array[b] = {
    val len = length
    val tmp = new Array[Array[b]](len)
    var i = 0
    while (i < len) {
      tmp(i) = f(apply(i))
      i = i + 1
    }
    Array.concat(tmp: _*)
  }

  final def zip[b](that: Array[b]): Array[Tuple2[Object,b]] = {
    val len = length
    if(len != that.length)
      throw new Error("zipping arrays of different length")
    val result = new Array[Tuple2[Object,b]](len)
    var i = 0
    while (i < len) {
      result(i) = new Tuple2(this(i), that(i))
      i = i + 1
    }
    result
  }

  final def zipWithIndex: Array[Tuple2[Object,Int]] = {
    val len = length
    val result = new Array[Tuple2[Object,Int]](len)
    var i = 0
    while (i < len) {
      result(i) = new Tuple2(this(i), i)
      i = i + 1
    }
    result
  }

  override final def stringPrefix: String = "Array"
}
