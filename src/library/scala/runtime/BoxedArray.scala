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
 * A class representing Array[T]
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

  def subArray(from: Int, end: Int): Object = null

  def filter(p: Any => Boolean): Object = null

  def map[b](f: Any => b): Array[b] = {
    val len = length
    val result = new Array[b](len)
    var i = 0
    while (i < len) {
      result(i) = f(apply(i))
      i = i + 1
    }
    result
  }

  def flatMap[b](f: Any => Array[b]): Array[b] = {
    val len = length
    val tmp = new Array[Array[b]](len)
    var i = 0
    while (i < len) {
      tmp(i) = f(apply(i))
      i = i + 1
    }
    Array.concat(tmp: _*)
  }

  override final def stringPrefix: String = "Array"
}
