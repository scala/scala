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
final class BoxedShortArray(val value: Array[Short]) extends BoxedArray[Short] {

  def elemManifest = ClassManifest.Short

  def length: Int = value.length

  def apply(index: Int): Short = value(index)

  def update(index: Int, elem: Short) {
    value(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = value
}
