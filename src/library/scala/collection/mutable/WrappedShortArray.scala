/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedShortArray.scala 18572 2009-08-25 14:14:11Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

@serializable
final class WrappedShortArray(val array: Array[Short]) extends WrappedArray[Short] {

  def elemManifest = ClassManifest.Short

  def length: Int = array.length

  def apply(index: Int): Short = array(index)

  def update(index: Int, elem: Short) {
    array(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = array
}
