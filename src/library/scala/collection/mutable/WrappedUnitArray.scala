/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedDoubleArray.scala 17680 2009-05-08 16:33:15Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

@serializable
final class WrappedUnitArray(val array: Array[Unit]) extends WrappedArray[Unit] {

  def elemManifest = ClassManifest.Unit

  def length: Int = array.length

  def apply(index: Int): Unit = array(index)

  def update(index: Int, elem: Unit) {
    array(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = array
}
