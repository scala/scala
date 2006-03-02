/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


/**
 * A class representing Array[T]
 */
abstract class BoxedArray extends PartialFunction[Int, Object] with Seq[Object] {
  /** The length of the array */
  def length: Int;

  /** The element at given index */
  def apply(index: Int): Object;

  /** Update element at given index */
  def update(index: Int, elem: Object): Unit;

  /** Convert to Java array.
   *  @param elemTag    Either one of the tags ".N" where N is the name of a primitive type
   *                    (@see ScalaRunTime), or a full class name.
   */
  def unbox(elemTag: String): Object;

  override def isDefinedAt(x: Int): Boolean   =  0 <= x && x < length;

  override def toString(): String = {
    val buf = new StringBuffer();
    buf.append("Array(");
    val len = length;
    var i = 0;
    while (i < len) { buf.append(apply(i)); i = i + 1 }
    buf.append(")");
    buf.toString()
  }

  def elements = new Iterator[Object] {
    var index = 0;
    def hasNext: Boolean = index < length;
    def next: Object = { val i = index; index = i + 1; apply(i) }
  }

  /** The underlying array value
   */
  def value: Object;

  def copyFrom(src: Object, from: Int, to: Int, len: Int): Unit =
    Array.copy(src, from, value, to, len)

  def copyTo(from: Int, dest: Object, to: Int, len: Int): Unit =
    Array.copy(value, from, dest, to, len)
}
