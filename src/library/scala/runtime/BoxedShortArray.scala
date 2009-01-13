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

@serializable
final class BoxedShortArray(val value: Array[Short]) extends BoxedArray {

  def length: Int = value.length

  def apply(index: Int): Any = Short.box(value(index))

  def update(index: Int, elem: Any) {
    value(index) = Short.unbox(elem.asInstanceOf[AnyRef])
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class[_]): AnyRef = value

  override def equals(other: Any) =
    value == other ||
    other.isInstanceOf[BoxedShortArray] && value == other.asInstanceOf[BoxedShortArray].value

  override def hashCode(): Int = value.hashCode()

  def subArray(start: Int, end: Int): Array[Short] = {
    val result = new Array[Short](end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  final override def filter(p: Any => Boolean): BoxedArray = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len += 1 }
      i += 1
    }
    val result = new Array[Short](len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len += 1 }
      i += 1
    }
    new BoxedShortArray(result)
  }
  override protected def newArray(length : Int, elements : Iterator[Any]) = {
    val result = new Array[Short](length)
    elements.map(_.asInstanceOf[Short]).copyToArray(result, 0)
    new BoxedShortArray(result)
  }
}
