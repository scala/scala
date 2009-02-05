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
final class BoxedDoubleArray(val value: Array[Double]) extends BoxedArray[Double] {

  def length: Int = value.length

  def apply(index: Int): Double = value(index)

  def update(index: Int, elem: Double) {
    value(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = value
}
