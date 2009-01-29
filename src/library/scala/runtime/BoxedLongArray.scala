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
final class BoxedLongArray(val value: Array[Long]) extends BoxedArray[Long] {

  def length: Int = value.length

  def apply(index: Int): Long = value(index)

  def update(index: Int, elem: Long) {
    value(index) = elem
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class[_]): AnyRef = value

  override def equals(other: Any) =
    value == other ||
    other.isInstanceOf[BoxedLongArray] && value == other.asInstanceOf[BoxedLongArray].value

  override def hashCode(): Int = value.hashCode()

  def subArray(start: Int, end: Int): Array[Long] = {
    val result = new Array[Long](end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  final override def filter(p: Long => Boolean): BoxedArray[Long] = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len += 1 }
      i += 1
    }
    val result = new Array[Long](len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len += 1 }
      i += 1
    }
    new BoxedLongArray(result)
  }
  override protected def newArray(length : Int, elements : Iterator[Long]) = {
    val result = new Array[Long](length)
    elements.map(_.asInstanceOf[Long]).copyToArray(result, 0)
    new BoxedLongArray(result)
  }


}
