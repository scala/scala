/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime
import scala.reflect.ClassManifest

@serializable
final class BoxedLongArray(val value: Array[Long]) extends BoxedArray[Long] {

  def elemManifest = ClassManifest.Long

  def length: Int = value.length

  def apply(index: Int): Long = value(index)

  def update(index: Int, elem: Long) {
    value(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = value
}
