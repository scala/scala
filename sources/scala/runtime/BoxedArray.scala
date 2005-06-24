/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

/** A class representing Array[T]
 */
abstract class BoxedArray() extends PartialFunction[Int, Object] with Seq[Object] {
  def length: Int;
  def apply(index: Int): Object;
  def update(index: Int, elem: Object): Unit;
  def unbox(elemClass: Class): Object;

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
}


