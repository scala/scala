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
import compat.Platform.createArray

@serializable
final class BoxedObjectArray[A <: AnyRef](val value: Array[AnyRef]) extends BoxedArray[A] {

  def length: Int = value.length

  def apply(index: Int): A = value(index).asInstanceOf[A]

  def update(index: Int, elem: A) {
    value(index) = elem
  }

  def unbox(elemTag: String): AnyRef = value
  def unbox(elemClass: Class[_]): AnyRef = value

/*
  override def equals(other: Any): Boolean =
    (value eq other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[BoxedObjectArray[_]] && (value eq other.asInstanceOf[BoxedObjectArray[_]].value)

  override def hashCode(): Int = (value.asInstanceOf[AnyRef]).hashCode()
*/

  private def create(length: Int): Array[AnyRef] = {
    createArray(value.getClass().getComponentType(), length).asInstanceOf[Array[AnyRef]]
  }

  final override def filter(p: A => Boolean): BoxedArray[A] = {
    val include = new Array[Boolean](value.length)
    var len = 0
    var i = 0
    while (i < value.length) {
      if (p(value(i).asInstanceOf[A])) { include(i) = true; len += 1 }
      i += 1
    }
    val result = create(len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len += 1 }
      i += 1
    }
    new BoxedObjectArray[A](result)
  }
}

