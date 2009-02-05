/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: BoxedByteArray.scala 17000 2009-01-29 13:05:53Z odersky $


package scalax.runtime

@serializable
final class BoxedShortArray(val value: Array[Short]) extends BoxedArray[Short] {

  def length: Int = value.length

  def apply(index: Int): Short = value(index)

  def update(index: Int, elem: Short) {
    value(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = value
}
