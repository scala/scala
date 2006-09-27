/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


[serializable]
final class BoxedShortArray(val value: Array[Short]) extends BoxedArray {

  def length: Int = value.length

  def apply(index: Int): Object = BoxedShort.box(value(index))

  def update(index: Int, elem: Object): Unit = {
    value(index) = elem.asInstanceOf[BoxedNumber].shortValue()
  }

  def unbox(elemTag: String): Object = value
  def unbox(elemClass: Class): Object = value

  override def equals(other: Any) =
    value == other ||
    other.isInstanceOf[BoxedShortArray] && value == other.asInstanceOf[BoxedShortArray].value

  override def hashCode(): Int = value.hashCode()

  def subArray(start: Int, end: Int): Array[Short] = {
    val result = new Array[Short](end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  def filter(p: Any => Boolean): Array[Short] = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len = len + 1 }
      i = i + 1
    }
    val result = new Array[Short](len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len = len + 1 }
      i = i + 1
    }
    result
  }
}

