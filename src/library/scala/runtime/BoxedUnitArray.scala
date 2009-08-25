/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: BoxedDoubleArray.scala 17680 2009-05-08 16:33:15Z odersky $


package scala.runtime
import scala.reflect.ClassManifest

@serializable
final class BoxedUnitArray(val value: Array[Unit]) extends BoxedArray[Unit] {

  def elemManifest = ClassManifest.Unit

  def length: Int = value.length

  def apply(index: Int): Unit = value(index)

  def update(index: Int, elem: Unit) {
    value(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = value
}
