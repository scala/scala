/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


[serializable]
final class BoxedObjectArray(val value: Array[Object]) extends BoxedArray {

  def length: Int = value.length

  def apply(index: Int): Object = value(index)

  def update(index: Int, elem: Object): Unit = { value(index) = elem }

  def unbox(elemTag: String): Object = value
  def unbox(elemClass: Class): Object = value

  override def equals(other: Any): Boolean =
    value == other ||
    other.isInstanceOf[BoxedObjectArray] && value == other.asInstanceOf[BoxedObjectArray].value

  override def hashCode(): Int = value.hashCode()

  private def create(length: Int): Array[Object] = {
    val elemClass = value.getClass().getComponentType()
    java.lang.reflect.Array.newInstance(elemClass, length).asInstanceOf[Array[Object]]
  }

  override def subArray(start: Int, end: Int): Array[Object] = {
    val result = create(end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  override def filter(p: Any => Boolean): Array[Object] = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len = len + 1 }
      i = i + 1
    }
    val result = create(len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len = len + 1 }
      i = i + 1
    }
    result
  }
}
