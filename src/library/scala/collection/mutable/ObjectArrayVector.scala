/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ObjectArrayVector.scala 18589 2009-08-27 14:45:35Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

import Predef._

@serializable
final class ObjectArrayVector[A <: AnyRef](val value: Array[AnyRef], val elemManifest: ClassManifest[A]) extends ArrayVector[A] {

// @deprecated("creating array w/o manifest")
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
    other.isInstanceOf[ObjectArrayVector[_]] && (value eq other.asInstanceOf[ObjectArrayVector[_]].value)

  override def hashCode(): Int = (value.asInstanceOf[AnyRef]).hashCode()
*/

}

