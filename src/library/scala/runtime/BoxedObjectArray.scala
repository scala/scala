/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._
import compat.Platform.createArray

@serializable
final class BoxedObjectArray(val value: Array[AnyRef]) extends BoxedArray {

  def length: Int = value.length

  def apply(index: Int): Any = value(index)

  def update(index: Int, elem: Any): Unit = {
    value(index) = elem.asInstanceOf[AnyRef]
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class): AnyRef = value

  override def equals(other: Any): Boolean =
    value == other ||
    other.isInstanceOf[BoxedObjectArray] && value == other.asInstanceOf[BoxedObjectArray].value

  override def hashCode(): Int = value.hashCode()

  private def create(length: Int): Array[AnyRef] = {
    createArray(value.getClass().getComponentType(), length).asInstanceOf[Array[AnyRef]]
  }

  override def subArray(start: Int, end: Int): Array[AnyRef] = {
    val result = create(end - start)
    Array.copy(value, start, result, 0, end - start)
    result
  }

  final override def filter(p: Any => Boolean): BoxedArray = {
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
    new BoxedObjectArray(result)
  }

  final override def slice(start: Int, end: Int): BoxedArray = {
    val len = end - start
    val result = create(len)
    Array.copy(value, start, result, 0, len)
    new BoxedObjectArray(result)
  }
}
