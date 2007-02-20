/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef.Class

@serializable
final class BoxedIntArray(val value: Array[Int]) extends BoxedArray {

  def length: Int = value.length

  def apply(index: Int): Any = Int.box(value(index))

  def update(index: Int, elem: Any): Unit = {
    value(index) = Int.unbox(elem.asInstanceOf[AnyRef])
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class): AnyRef = value

  override def equals(other: Any) =
    value == other ||
    other.isInstanceOf[BoxedIntArray] && value == other.asInstanceOf[BoxedIntArray].value

  override def hashCode(): Int = value.hashCode()

  def subArray(start: Int, end: Int): Array[Int] = {
    val result = new Array[Int](end - start)
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
    val result = new Array[Int](len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len = len + 1 }
      i = i + 1
    }
    new BoxedIntArray(result)
  }

  final override def slice(start: Int, end: Int): BoxedArray = {
    val len = end - start
    val result = new Array[Int](len)
    Array.copy(value, start, result, 0, len)
    new BoxedIntArray(result)
  }
}
