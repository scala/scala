/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedCharArray.scala 18572 2009-08-25 14:14:11Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

@serializable
final class WrappedCharArray(val array: Array[Char]) extends WrappedArray[Char] {

  def elemManifest = ClassManifest.Char

  def length: Int = array.length

  def apply(index: Int): Char = array(index)

  def update(index: Int, elem: Char) {
    array(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = array
}
