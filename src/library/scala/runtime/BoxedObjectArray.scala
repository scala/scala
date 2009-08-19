/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime
import scala.reflect.Manifest

import Predef._

@serializable
final class BoxedObjectArray[A <: AnyRef](val value: Array[AnyRef], val elemManifest: Manifest[A]) extends BoxedArray[A] {

  def this(value: Array[AnyRef]) = this(value, null) // !!! todo: remove

  def length: Int = value.length

  def apply(index: Int): A = value(index).asInstanceOf[A]

  def update(index: Int, elem: A) {
    value(index) = elem
  }

  def unbox(elemClass: Class[_]): AnyRef = value

/*
  override def equals(other: Any): Boolean =
    (value eq other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[BoxedObjectArray[_]] && (value eq other.asInstanceOf[BoxedObjectArray[_]].value)

  override def hashCode(): Int = (value.asInstanceOf[AnyRef]).hashCode()
*/

}

