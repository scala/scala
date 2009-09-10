/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedObjectArray.scala 18589 2009-08-27 14:45:35Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

import Predef._

@serializable
final class WrappedRefArray[A](val array: Array[AnyRef]) extends WrappedArray[A] {

  lazy val elemManifest = ClassManifest.classType[A](array.getClass.getComponentType)

  def length: Int = array.length

  def apply(index: Int): A = array(index).asInstanceOf[A]

  def update(index: Int, elem: A) {
    array(index) = elem.asInstanceOf[AnyRef]
  }

  def unbox(elemClass: Class[_]): AnyRef = array
}

