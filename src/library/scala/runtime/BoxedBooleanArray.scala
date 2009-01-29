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
final class BoxedBooleanArray(val value: Array[Boolean]) extends BoxedArray[Boolean] {

  def length: Int = value.length

  def apply(index: Int): Boolean = value(index)

  def update(index: Int, elem: Boolean) {
    value(index) = elem
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class[_]): AnyRef = value

  override def equals(other: Any) =
    value == other ||
    other.isInstanceOf[BoxedBooleanArray] && value == other.asInstanceOf[BoxedBooleanArray].value

  override def hashCode(): Int = value.hashCode()

  def subArray(start: Int, end: Int): Array[Boolean] = {
    val result = new Array[Boolean](end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  final override def filter(p: Boolean => Boolean): BoxedArray[Boolean] = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len += 1 }
      i += 1
    }
    val result = new Array[Boolean](len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len += 1 }
      i += 1
    }
    new BoxedBooleanArray(result)
  }
  override protected def newArray(length : Int, elements : Iterator[Boolean]) = {
    val result = new Array[Boolean](length)
    elements.map(_.asInstanceOf[Boolean]).copyToArray(result, 0)
    new BoxedBooleanArray(result)
  }
}
